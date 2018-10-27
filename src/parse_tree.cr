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
    
    # the following query methods are useful for exploring and querying the parse tree

    def labeled?
      !!@label
    end

    def root?
      @parent.nil?
    end

    def preorder_traverse(visit : ParseTree -> _)
      visit.call(self)
      children.each {|child| child.preorder_traverse(visit) }
    end

    def postorder_traverse(visit : ParseTree -> _)
      children.each {|child| child.preorder_traverse(visit) }
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

    def term : ParseTree
      terms.first
    end

    def term? : ParseTree
      terms.first?
    end

    # "terms", as opposed to children, returns the list of descendant nodes that logically form the children of a 
    # given ParseTree node
    # Another way of thinking about this is #children returns concrete children, #terms returns logical children.
    # In practical terms, only ApplyTree and ChoiceTree differ in their treatment of #children vs. #terms
    def terms : Array(ParseTree)
      raise "ParseTree#terms is an abstract method"
    end

    def terminal?
      false
    end

    # returns the matched substring of the input that this parse tree node represents
    def text
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

    # query methods

    def children : Array(ParseTree)
      [tree] of ParseTree
    end

    def terms : Array(ParseTree)
      tree.terms
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

    # query methods

    def children : Array(ParseTree)
      [tree] of ParseTree
    end

    def terms : Array(ParseTree)
      tree.terms
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

    # query methods

    def children : Array(ParseTree)
      seq
    end

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

    # query methods

    def terminal?
      true
    end

    def children : Array(ParseTree)
      [] of ParseTree
    end

    def terms : Array(ParseTree)
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

    # query methods

    def terminal?
      true
    end

    def children : Array(ParseTree)
      [] of ParseTree
    end

    def terms : Array(ParseTree)
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

    # query methods

    def children : Array(ParseTree)
      raise "NegLookAheadTree#children undefined"
    end

    def terms : Array(ParseTree)
      raise "NegLookAheadTree#terms undefined"
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

    # query methods

    def children : Array(ParseTree)
      raise "PosLookAheadTree#children undefined"
    end

    def terms : Array(ParseTree)
      raise "PosLookAheadTree#terms undefined"
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

    # query methods

    def children : Array(ParseTree)
      [tree].compact
    end

    # We're treating Optional and Repetition expression nodes the same in this respect, they both represent a 
    # repetition of expression nodes, therefore, #term will return an array of expression nodes matched
    # by the repetition expression.
    def terms : Array(ParseTree)
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

    # query methods

    def children : Array(ParseTree)
      trees
    end

    def terms : Array(ParseTree)
      trees
    end
  end
end