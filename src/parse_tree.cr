module Arborist
  alias SyntaxTree = String | Array(SyntaxTree)

  # A nil parse tree means parse error
  # ParseTree is an abstract class that defines one required field: finishing_pos : Int32
  class ParseTree
    property label : String?
    property finishing_pos : Int32  # the position within the input string that points at the last character this parse tree captures
    
    def initialize
      @finishing_pos = -1
      raise "ParseTree#initialize is an abstract class"
    end

    def syntax_tree : SyntaxTree
      raise "ParseTree#syntax_tree is an abstract class"
    end
  end

  class ApplyTree < ParseTree
    property tree : ParseTree

    def initialize(@tree, @finishing_pos)
    end

    def label(label : String?) : ApplyTree
      @label = label
      self
    end

    def syntax_tree : SyntaxTree
      tree.syntax_tree
    end
  end

  class TerminalTree < ParseTree
    property str : String
    
    def initialize(@str, @finishing_pos)
    end
    
    def label(label : String?) : TerminalTree
      @label = label
      self
    end

    def syntax_tree : SyntaxTree
      @str
    end
  end

  class ChoiceTree < ParseTree
    property tree : ParseTree

    def initialize(@tree, @finishing_pos)
    end

    def label(label : String?) : ChoiceTree
      @label = label
      self
    end

    def syntax_tree : SyntaxTree
      tree.syntax_tree
    end
  end

  class SequenceTree < ParseTree
    property seq : Array(ParseTree)
    
    def initialize(@seq, @finishing_pos)
    end

    def label(label : String?) : SequenceTree
      @label = label
      self
    end

    def syntax_tree : SyntaxTree
      seq.map {|n| n.syntax_tree.as(SyntaxTree) }
    end
  end

  class NegLookAheadTree < ParseTree
    property succeed : Bool

    def initialize(@succeed)
      @finishing_pos = -1
    end

    def label(label : String?) : NegLookAheadTree
      @label = label
      self
    end

    def syntax_tree : SyntaxTree
      raise "No syntax tree equivalent for NegLookAhead"
    end
  end

  class PosLookAheadTree < ParseTree
    property succeed : Bool

    def initialize(@succeed)
      @finishing_pos = -1
    end

    def label(label : String?) : PosLookAheadTree
      @label = label
      self
    end

    def syntax_tree : SyntaxTree
      raise "No syntax tree equivalent for PosLookAhead"
    end
  end

  class OptionalTree < ParseTree
    property tree : ParseTree?

    def initialize(@tree, @finishing_pos)
    end

    def label(label : String?) : OptionalTree
      @label = label
      self
    end

    def syntax_tree : SyntaxTree
      (tree = @tree) ? [tree.syntax_tree] of SyntaxTree : [] of SyntaxTree
    end
  end

  class RepetitionTree < ParseTree
    property trees : Array(ParseTree)

    def initialize(@trees, @finishing_pos)
    end

    def label(label : String?) : RepetitionTree
      @label = label
      self
    end

    def syntax_tree : SyntaxTree
      trees.map {|n| n.syntax_tree.as(SyntaxTree) }
    end
  end
end