module Arborist
  class MemoizedParseTree
    property parse_tree : ParseTree?    # the parse tree matched at the index position within the memotable array at which this MemoizedParseTree exists
    property next_pos : Int32

    def initialize(@parse_tree = nil, @next_pos = 0)
    end
  end

  # RuleToMemoizedParseTree is a Map from Rule => memoized-parse-tree-with-next-pos-state
  alias RuleToMemoizedParseTree = Hash(Rule, MemoizedParseTree)

  alias MemoTable = Hash(Int32, RuleToMemoizedParseTree)

  class MemoTree
    # stores the memoized parse trees that occur in the rule-in-recursion call stack state represented by this MemoTree node
    property memo_table : Hash(Int32, RuleToMemoizedParseTree)

    # stores memoization tables for ApplyCalls that occur in a more specific rule-in-recursion call stack state than is represented by this MemoTree node
    # The tuple representing the key is of the form: {pos, rule_name}
    property children : Hash({Int32, Rule}, MemoTree)

    property call_stack_to_memo_tree : Hash( Array({Int32, Rule}), MemoTree )

    def self.new_with_same_memo_table(other_memo_tree)
      mt = MemoTree.new
      other_memo_tree.memo_table.each do |pos, col|
        mt.memo_table[pos] = col.dup    # shallow copy of RuleToMemoizedParseTree
      end
      mt
    end

    def initialize()
      @memo_table = {} of Int32 => RuleToMemoizedParseTree
      @children = {} of {Int32, Rule} => MemoTree
      @call_stack_to_memo_tree = {} of Array({Int32, Rule}) => MemoTree
      @call_stack_to_memo_tree[Array({Int32, Rule}).new] = self
    end

    def exist?(rule_in_recursion_call_stack_state : Array({Int32, Rule}), pos : Int32, rule : Rule) : Bool
      # tree_node : MemoTree = self
      # rule_in_recursion_call_stack_state.each do |pos_rule_name_pair|
      #   tree_node = tree_node.children[pos_rule_name_pair]? || (return false)
      # end

      # tree_node.local_exist?(pos, rule_name)
      
      tree_node = @call_stack_to_memo_tree[rule_in_recursion_call_stack_state]? || (return false)   # todo: I think this is wrong
      tree_node.local_exist?(pos, rule)
    end

    def local_exist?(pos : Int32, rule : Rule) : Bool
      col = self.memo_table[pos]?
      (col && col.has_key?(rule)) || false
    end

    def lookup(rule_in_recursion_call_stack_state : Array({Int32, Rule}), pos : Int32, rule : Rule) : MemoizedParseTree?
      # tree_node : MemoTree = self
      # rule_in_recursion_call_stack_state.each do |pos_rule_name_pair|
      #   tree_node = tree_node.children[pos_rule_name_pair]? || (return nil)
      # end
      # memo_table = tree_node.memo_table
      # col = memo_table[pos]?
      # col[rule_name]? if col

      tree_node : MemoTree = @call_stack_to_memo_tree[rule_in_recursion_call_stack_state]? || (return nil)    # todo: I think this is wrong
      memo_table = tree_node.memo_table
      col = memo_table[pos]?
      col[rule]? if col
    end

    def local_lookup(pos : Int32, rule : Rule) : MemoizedParseTree
      self.memo_table[pos][rule]
    end


    def add(rule_in_recursion_call_stack_state : Array({Int32, Rule}), pos : Int32, rule : Rule, memo_result : MemoizedParseTree) : MemoizedParseTree
      tree_node = self
      rule_in_recursion_call_stack_state.each do |pos_rule_pair|
        tree_node = (tree_node.children[pos_rule_pair] ||= MemoTree.new_with_same_memo_table(tree_node))
      end

      @call_stack_to_memo_tree[rule_in_recursion_call_stack_state] = tree_node

      tree_node.local_memoize(pos, rule, memo_result)
    end

    def local_memoize(pos : Int32, rule : Rule, memo_result : MemoizedParseTree) : MemoizedParseTree
      memo_table = self.memo_table
      col = (memo_table[pos] ||= RuleToMemoizedParseTree.new)
      col[rule] = memo_result
    end

  end
end