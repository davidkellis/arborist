# Design of semantic actions and parse tree nodes is very similar to the design of semantic actions in Antlr4, treetop, and citrus.
# See also:
# http://cjheath.github.io/treetop/
# https://github.com/mjackson/citrus
# https://github.com/cjheath/treetop/blob/master/lib/treetop/runtime/syntax_node.rb
# https://github.com/antlr/antlr4/blob/master/doc/parser-rules.md
# http://cjheath.github.io/treetop/semantic_interpretation.html
# https://github.com/antlr/antlr4/issues/1409

require "msgpack"

module Arborist
  alias SyntaxTree = String | Array(SyntaxTree) | Nil

  # A nil parse tree means parse error
  # ParseTree is an abstract base class that defines one required field: finishing_pos : Int32
  class ParseTree   # rename this to SyntaxNode
    property input : String
    property label : String?
    property start_pos : Int32      # the position within the input string that points at teh first character this parse tree captures
    property finishing_pos : Int32  # the position within the input string that points at the last character this parse tree captures
    property parent : ParseTree?
    @captures : Hash(String, Array(ParseTree))?
    @local_captures : Hash(String, Array(ParseTree))?

    # This method returns a serialized representation of the specified parse tree, encoded with MessagePack.
    # The serialization layout is:
    # <input><array of ParseTree nodes>
    # Each ParseTree node is represented with the following structure:
    # <node type (ApplyTree | TerminalTree | ...)>
    # <node ID>
    # <input start position (inclusive)>
    # <input end position (inclusive)>
    # <node label (may be nil)>
    # <parent node ID (may be nil)>
    # <array of children node IDs>
    # <array of captures, where each capture is a 4-tuple (String,Int32,Int32,Int32), 
    #   representing (capture name, node ID belonging to referenced node, input start position inclusive, input end position inclusive) >
    # <node attributes specific to the type of node (e.g. ApplyTree has a `rule_name` attribute; TerminalTree has a `str` attribute; etc.)>
    def self.to_msgpack(parse_tree : ParseTree) : Bytes
      packer = MessagePack::Packer.new
      parse_tree.input.to_msgpack(packer)
      parse_tree.postorder_traverse do |parse_tree_node|
        parse_tree_node.to_msgpack(packer)
      end
      packer.to_slice
    end

    def initialize
      @input = ""
      @label = nil
      @start_pos = -1
      @finishing_pos = -1
      @parent = nil
      raise "ParseTree#initialize is an abstract method"
    end

    def syntax_tree : SyntaxTree
      raise "ParseTree#syntax_tree is an abstract method"
    end

    # call this on the root node in the tree to update all the nodes in the tree with their parent node
    def recursively_populate_parents(parent : ParseTree? = nil)
      @parent = parent
      children.each(&.recursively_populate_parents(self))
      self
    end

    def s_exp(indent : Int32 = 0) : String
      raise "ParseTree#s_exp is an abstract method"
    end

    # Each ParseTree node is represented with the following structure:
    # <node type (ApplyTree | TerminalTree | ...)>
    # <node ID>
    # <input start position (inclusive)>
    # <input end position (inclusive)>
    # <node label (may be nil)>
    # <parent node ID (may be nil)>
    # <array of children node IDs>
    # <map of captures, having type Map(String, Array(UInt64)), representing:
    #   <capture name> => [node ID belonging to referenced node, another node ID belonging to referenced node, ...],
    #   <capture name> => [node ID belonging to referenced node, another node ID belonging to referenced node, ...],
    #   ...
    # >
    # <node attributes specific to the type of node (e.g. ApplyTree has a `rule_name` attribute; TerminalTree has a `str` attribute; etc.)>
    def to_msgpack(packer : MessagePack::Packer)
      self.class.name.split(":").last.to_msgpack(packer)    # all the class names are prefixed with Arborist::, like: Arborist::ApplyTree
      object_id.to_msgpack(packer)
      start_pos.to_msgpack(packer)
      finishing_pos.to_msgpack(packer)
      label.to_msgpack(packer)
      if parent = self.parent
        parent.object_id.to_msgpack(packer)
      else
        nil.to_msgpack(packer)
      end
      children.map(&.object_id).to_msgpack(packer)
      captures.map do |capture_name, parse_tree_nodes|
        parse_tree_node_ids = parse_tree_nodes.map(&.object_id)
        [capture_name, parse_tree_node_ids]
      end.to_h.to_msgpack(packer)
      parse_tree_type_specific_attributes_to_msgpack(packer)
    end

    def parse_tree_type_specific_attributes_to_msgpack(packer : MessagePack::Packer)
      # default implementation is a No-Op
    end

    
    # the following query methods are useful for exploring and querying the parse tree

    def labeled?
      !!@label
    end

    # returns the name of the rule that, as a result of being evaluated, yielded this parse tree node
    def enclosing_rule_name : String?
      parent = parent()
      if parent
        if parent.is_a?(ApplyTree)
          parent.rule_name
        else
          parent.enclosing_rule_name
        end
      else
        if self.is_a?(ApplyTree)
          self.rule_name
        end
      end
    end

    def root?
      @parent.nil?
    end

    def preorder_traverse(visit : ParseTree -> _)
      visit.call(self)
      children.each {|child| child.preorder_traverse(visit) }
    end

    def postorder_traverse(&visit : ParseTree -> _)
      postorder_traverse(visit)
    end

    def postorder_traverse(visit : ParseTree -> _)
      children.each {|child| child.postorder_traverse(visit) }
      visit.call(self)
    end

    def capture(name : String) : ParseTree
      capture?(name) || raise("Unknown capture reference: #{name}")
    end

    def capture?(name : String) : ParseTree?
      captures(name).first?
    end

    def captures(name : String) : Array(ParseTree)
      captures[name]? || [] of ParseTree
    end
    
    def captures : Hash(String, Array(ParseTree))
      @captures ||= children.reject(&.is_a?(ApplyTree)).reduce(local_captures()) do |captures, child|
        child.captures.each do |child_capture_string, child_captures|
          captures[child_capture_string] ||= [] of ParseTree
          captures[child_capture_string].concat(child_captures)
        end
        captures
      end
    end

    def local_captures : Hash(String, Array(ParseTree))
      @local_captures ||= children.reduce({} of String => Array(ParseTree)) do |captures, child|
        child_tree_node_label = child.label || (child.is_a?(ApplyTree) && child.rule_name)
        if child_tree_node_label
          child_tree_node_label = child_tree_node_label.as(String)
          captures[child_tree_node_label] ||= [] of ParseTree
          captures[child_tree_node_label] << child
        end
        captures
      end
    end

    def child : ParseTree
      children.first
    end

    def child? : ParseTree?
      children.first?
    end

    # children returns the list of all actual/literal child ParseTree nodes in parse tree structure, 
    # rather than skipping some, as with #terms
    # Another way of thinking about this is #children returns concrete children, #terms returns logical children.
    def children : Array(ParseTree)
      raise "ParseTree#children is an abstract method"
    end

    # returns all descendants listed out in a pre-order traversal
    def descendants : Array(ParseTree)
      nodes = [] of ParseTree
      accumulate = ->(node : ParseTree) { nodes << node }
      children.each {|child| child.preorder_traverse(accumulate) }
      nodes
    end

    # returns all nodes listed out in a pre-order traversal
    def self_and_descendants : Array(ParseTree)
      ([self] of ParseTree).concat(descendants)
    end

    def terminal?
      false
    end

    # returns the matched substring of the input that this parse tree node represents
    def text : String
      if finishing_pos < start_pos    # this indicates an expr matched zero text, so we return the empty string
        ""
      else
        input[start_pos..finishing_pos]
      end
    end

    def visit(visitor : Visitor(R)) forall R
      raise "ParseTree#visit is only defined for ApplyTree nodes."
    end
  end

  class ApplyTree < ParseTree
    property rule_name : String
    property tree : ParseTree

    def initialize(@tree, @rule_name, @input, @start_pos, @finishing_pos)
    end

    def label(label : String?) : ApplyTree
      @label = label
      self
    end

    def syntax_tree : SyntaxTree
      tree.syntax_tree
    end

    def s_exp(indent : Int32 = 0) : String
      prefix = " " * indent
      "#{prefix}(apply #{rule_name} ; id=#{object_id} rule_name=\"#{rule_name}\" label=\"#{label}\"\n#{tree.s_exp(indent+2)})"
    end

    def parse_tree_type_specific_attributes_to_msgpack(packer : MessagePack::Packer)
      rule_name.to_msgpack(packer)
    end

    # query methods

    def children : Array(ParseTree)
      [tree] of ParseTree
    end

    def visit(visitor : Visitor(R)) : R forall R
      visitor.visit(self)
    end
  end

  class ChoiceTree < ParseTree
    property tree : ParseTree

    def initialize(@tree, @input, @start_pos, @finishing_pos)
    end

    def label(label : String?) : ChoiceTree
      @label = label
      self
    end

    def syntax_tree : SyntaxTree
      tree.syntax_tree
    end

    def s_exp(indent : Int32 = 0) : String
      prefix = " " * indent
      "#{prefix}(choice ; id=#{object_id} label=\"#{label}\"\n#{tree.s_exp(indent+2)})"
    end

    # query methods

    def children : Array(ParseTree)
      [tree] of ParseTree
    end
  end

  class SequenceTree < ParseTree
    property seq : Array(ParseTree)
    
    def initialize(@seq, @input, @start_pos, @finishing_pos)
    end

    def label(label : String?) : SequenceTree
      @label = label
      self
    end

    def syntax_tree : SyntaxTree
      # seq.map {|pt| pt.try(&.syntax_tree) }.compact  # <-- this is what I'd like to write, but I can't make the compiler accept this
      nodes = [] of SyntaxTree
      seq.each {|pt| syntax_tree = pt.try(&.syntax_tree).as(SyntaxTree); nodes << syntax_tree if syntax_tree }
      nodes
    end

    def s_exp(indent : Int32 = 0) : String
      prefix = " " * indent
      "#{prefix}(seq ; id=#{object_id} label=\"#{label}\"\n#{seq.map(&.s_exp(indent+2)).join("\n")})"
    end

    # query methods

    def children : Array(ParseTree)
      seq
    end
  end

  class TerminalTree < ParseTree
    property str : String
    
    def initialize(@str, @input, @start_pos, @finishing_pos)
    end
    
    def label(label : String?) : TerminalTree
      @label = label
      self
    end

    def syntax_tree : SyntaxTree
      @str
    end

    def s_exp(indent : Int32 = 0) : String
      prefix = " " * indent
      "#{prefix}\"#{@str}\"[id=#{object_id} label=\"#{label}\"]"
    end

    def parse_tree_type_specific_attributes_to_msgpack(packer : MessagePack::Packer)
      str.to_msgpack(packer)
    end

    # query methods

    def terminal?
      true
    end

    def children : Array(ParseTree)
      [] of ParseTree
    end
  end

  class MutexAltTree < ParseTree
    property str : String
    
    def initialize(@str, @input, @start_pos, @finishing_pos)
    end
    
    def label(label : String?) : MutexAltTree
      @label = label
      self
    end

    def syntax_tree : SyntaxTree
      @str
    end

    def s_exp(indent : Int32 = 0) : String
      prefix = " " * indent
      "#{prefix}\"#{@str}\"MA[id=#{object_id} label=\"#{label}\"]"
    end

    def parse_tree_type_specific_attributes_to_msgpack(packer : MessagePack::Packer)
      str.to_msgpack(packer)
    end

    # query methods

    def terminal?
      true
    end

    def children : Array(ParseTree)
      [] of ParseTree
    end
  end

  class NegLookAheadTree < ParseTree
    property succeed : Bool

    def initialize(@succeed, @input, @start_pos)
      @finishing_pos = -1
    end

    def label(label : String?) : NegLookAheadTree
      @label = label
      self
    end

    def syntax_tree : SyntaxTree
      raise "NegLookAheadTree#syntax_tree undefined"
    end

    def s_exp(indent : Int32 = 0) : String
      raise "NegLookAheadTree#s_exp undefined"
    end

    def parse_tree_type_specific_attributes_to_msgpack(packer : MessagePack::Packer)
      raise "NegLookAheadTree#parse_tree_type_specific_attributes_to_msgpack undefined"
    end

    # query methods

    def children : Array(ParseTree)
      raise "NegLookAheadTree#children undefined"
    end
  end

  class PosLookAheadTree < ParseTree
    property succeed : Bool

    def initialize(@succeed, @input, @start_pos)
      @finishing_pos = -1
    end

    def label(label : String?) : PosLookAheadTree
      @label = label
      self
    end

    def syntax_tree : SyntaxTree
      raise "PosLookAhead#syntax_tree undefined"
    end

    def s_exp(indent : Int32 = 0) : String
      raise "PosLookAheadTree#s_exp undefined"
    end

    def parse_tree_type_specific_attributes_to_msgpack(packer : MessagePack::Packer)
      raise "PosLookAheadTree#parse_tree_type_specific_attributes_to_msgpack undefined"
    end

    # query methods

    def children : Array(ParseTree)
      raise "PosLookAheadTree#children undefined"
    end
  end

  class OptionalTree < ParseTree
    property tree : ParseTree?

    def initialize(@tree, @input, @start_pos, @finishing_pos)
    end

    def label(label : String?) : OptionalTree
      @label = label
      self
    end

    def syntax_tree : SyntaxTree
      @tree.try(&.syntax_tree)
    end

    def s_exp(indent : Int32 = 0) : String
      tree = @tree
      if tree
        prefix = " " * indent
        "#{prefix}(opt ; id=#{object_id} label=\"#{label}\"\n#{tree.s_exp(indent+2)})"
      else
        ""
      end
    end

    # query methods

    def children : Array(ParseTree)
      [tree].compact
    end
  end

  class RepetitionTree < ParseTree
    property trees : Array(ParseTree)

    def initialize(@trees, @input, @start_pos, @finishing_pos)
    end

    def label(label : String?) : RepetitionTree
      @label = label
      self
    end

    def syntax_tree : SyntaxTree
      trees.map {|n| n.syntax_tree.as(SyntaxTree) }
    end

    def s_exp(indent : Int32 = 0) : String
      prefix = " " * indent
      "#{prefix}(repeat ; id=#{object_id} label=\"#{label}\"\n#{trees.map(&.s_exp(indent+2)).join("\n")})"
    end

    # query methods

    def children : Array(ParseTree)
      trees
    end
  end
end