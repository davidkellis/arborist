module Arborist
  class RootMemoizationScope
    property memo_table : MemoTable

    def initialize
      @memo_table = MemoTable.new
    end

    def local_exist?(pos : Int32, rule : Rule) : Bool
      col = self.memo_table[pos]?
      (col && col.has_key?(rule)) || false
    end

    def local_lookup(pos : Int32, rule : Rule) : MemoizedParseTree
      self.memo_table[pos][rule]
    end

    def local_memoize(pos : Int32, rule : Rule, memo_result : MemoizedParseTree) : MemoizedParseTree
      memo_table = self.memo_table
      col = (memo_table[pos] ||= RuleToMemoizedParseTree.new)
      col[rule] = memo_result
    end

  end

  class ExprCallTree
    property expr_call : ExprCall
    property parent : ExprCallTree?
    property parent_apply_node : ApplyCallTree?
    property children : Array(ExprCallTree)

    def initialize(@expr_call, @parent, @parent_apply_node)
      @children = [] of ExprCallTree
    end

    # returns the newly created child tree node
    def append_child(expr_call : ExprCall, root_memoization_scope : RootMemoizationScope)
      if expr_call.is_a?(ApplyCall)
        append_child(ApplyCallTree.new(expr_call, self, parent_apply_node, parent_apply_node || root_memoization_scope))
      else
        append_child(ExprCallTree.new(expr_call, self, parent_apply_node))
      end
    end

    # returns the child tree node
    def append_child(child_tree : ExprCallTree)
      @children << child_tree
      child_tree
    end

    def self_and_descendants
      nodes = [] of ExprCallTree
      postorder_traverse do |expr_call_tree|
        nodes << expr_call_tree
      end
      nodes
    end

    def postorder_traverse(&blk : ExprCallTree -> )
      children.each {|child| child.postorder_traverse(&blk) }
      blk.call(self)
    end

    def hash(hasher)
      self.object_id.hash(hasher)
    end

  end

  class ApplyCallTree < ExprCallTree
    property memo_table : MemoTable

    def initialize(expr_call, parent, parent_apply_node, base_memoization_scope)
      super(expr_call, parent, parent_apply_node)
      # @memo_table = base_memoization_scope.memo_table.dup    # does the same as #clone_memoization_scope(base_memoization_scope)
      @memo_table = MemoTable.new   # todo: what effect does this have on performance? the one above seems more correct, but I don't know what the actual effect is
    end
    
    def apply_call : ApplyCall
      expr_call.as(ApplyCall)
    end

    def append_child(expr_call : ExprCall, root_memoization_scope : RootMemoizationScope)
      if expr_call.is_a?(ApplyCall)
        append_child(ApplyCallTree.new(expr_call, self, self, self))
      else
        append_child(ExprCallTree.new(expr_call, self, self))
      end
    end


    # memoization logic

    def clone_memoization_scope(other_memoization_scope)
      @memo_table = other_memoization_scope.memo_table.dup
    end


    def local_exist?(pos : Int32, rule : Rule) : Bool
      col = self.memo_table[pos]?
      (col && col.has_key?(rule)) || false
    end

    def local_lookup(pos : Int32, rule : Rule) : MemoizedParseTree
      self.memo_table[pos][rule]
    end

    def local_memoize(pos : Int32, rule : Rule, memo_result : MemoizedParseTree) : MemoizedParseTree
      memo_table = self.memo_table
      col = (memo_table[pos] ||= RuleToMemoizedParseTree.new)
      col[rule] = memo_result
    end

  end

  class ExprCallTreeController
    property root : ExprCallTree?
    property current_node : ExprCallTree?
    property current_apply_node : ApplyCallTree?

    @apply_call_stack : Array(ApplyCall)
    @apply_call_stacks_per_rule : Hash(Rule, Array(ApplyCall))
    @apply_calls_per_rule_and_pos : Hash(Tuple(Rule, Int32), Array(ApplyCall))

    @root_memoization_scope : RootMemoizationScope

    def initialize()
      @root = nil
      @current_node = nil
      @current_apply_node = nil
    
      @apply_call_stack = [] of ApplyCall
      @apply_call_stacks_per_rule = Hash(Rule, Array(ApplyCall)).new
      @apply_calls_per_rule_and_pos = Hash(Tuple(Rule, Int32), Array(ApplyCall)).new

      @root_memoization_scope = RootMemoizationScope.new
    end

    def reset
      @root = nil
      @current_node = nil
      @current_apply_node = nil
    
      @apply_call_stack = [] of ApplyCall
      @apply_call_stacks_per_rule = Hash(Rule, Array(ApplyCall)).new
      @apply_calls_per_rule_and_pos = Hash(Tuple(Rule, Int32), Array(ApplyCall)).new

      @root_memoization_scope = RootMemoizationScope.new
    end

    def push_onto_call_stack(expr_call : ExprCall)
      new_current_node = if current_node = @current_node
        @current_node = current_node.append_child(expr_call, @root_memoization_scope)
      else
        @root = if expr_call.is_a?(ApplyCall)
          ApplyCallTree.new(expr_call, nil, nil, @root_memoization_scope)
        else
          # ExprCallTree.new(expr_call, nil, nil)
          raise "The root node of the expression call tree should be an ApplyCallTree."
        end
        @current_node = @root
      end
      
      @current_apply_node = new_current_node if new_current_node.is_a?(ApplyCallTree)

      # maintain call stack structures
      if expr_call.is_a?(ApplyCall)
        @apply_call_stack.push(expr_call)

        apply_call_stack_for_rule = (@apply_call_stacks_per_rule[expr_call.rule] ||= [] of ApplyCall)
        apply_call_stack_for_rule.push(expr_call)

        apply_call_stack_for_rule_and_pos = (@apply_calls_per_rule_and_pos[{expr_call.rule, expr_call.pos}] ||= [] of ApplyCall)
        apply_call_stack_for_rule_and_pos.push(expr_call)
      end

      expr_call
    end

    # this assumes that popping off will work
    def pop_off_of_call_stack() : ExprCall
      popped_expr_call : ExprCall? = nil
      
      if top_node = @current_node
        # maintain call stack structures
        popped_expr_call = top_node.expr_call
        if popped_expr_call.is_a?(ApplyCall)
          @apply_call_stack.pop
          @apply_call_stacks_per_rule[popped_expr_call.rule].pop()
          @apply_calls_per_rule_and_pos[{popped_expr_call.rule, popped_expr_call.pos}].pop()
        end

        @current_node = top_node.parent
        @current_apply_node = top_node.parent_apply_node
        reset unless @current_node
      end

      popped_expr_call || raise "ExpressionCallTree#pop_off_of_call_stack called when the call stack was empty!"
    end

    def current_expr_call_failed()
      # @current_node and all descendant nodes should be removed from the child_recursive_calls of any ancestor node
      #
      # I will implement this by having each ExprCall that is added as a child_recursive_call also track which parent
      # it was added as a child of, and then the ExprCall that was added as a child will know both the parent and child
      # nodes in which the child needs to be removed from the parent's child_recursive_call set.
      if current_node = @current_node
        expr_calls_that_descend_from_current_failed_expr_call_tree = current_node.self_and_descendants.map(&.expr_call)
        expr_calls_that_descend_from_current_failed_expr_call_tree.each do |expr_call|
          expr_call.remove_self_from_parent_recursive_call if expr_call.is_a?(ApplyCall)
        end
      end
    end


    # call stack methods

    def apply_calls_in_call_stack : Array(ApplyCall)
      # if top_apply_node = @current_apply_node
      #   nodes = [] of ApplyCall
      #   node : ApplyCallTree? = top_apply_node
      #   while node
      #     nodes << node.apply_call
      #     node = node.parent_apply_node
      #   end
      #   nodes.reverse!
      # else
      #   [] of ApplyCall
      # end
      @apply_call_stack
    end

    def apply_calls_that_resulted_in_left_recursion : Array(ApplyCall)
      apply_calls_in_call_stack.select(&.resulted_in_left_recursion?)
    end

    def left_recursive_apply_calls : Array(ApplyCall)
      apply_calls_in_call_stack.select(&.left_recursive?)
    end

    def lookup_rule_application_in_call_stack(rule : Rule, pos : Int32) : ApplyCall?
      @apply_calls_per_rule_and_pos[{rule, pos}]?.try(&.last?)
    end

    # returns the deepest/most-recent application of `rule` in the rule application stack
    def lookup_most_recent_rule_application_in_call_stack(rule : Rule) : ApplyCall?
      apply_call_stack_for_rule = @apply_call_stacks_per_rule[rule]?
      apply_call_stack_for_rule.last? if apply_call_stack_for_rule
    end

    # returns the leftmost/earliest/oldest/shallowest application of `rule` in the rule application stack that resulted in left recursion
    def lookup_oldest_rule_application_that_resulted_in_left_recursion(rule) : ApplyCall?
      apply_calls_that_resulted_in_left_recursion.find {|apply_call| apply_call.rule == rule }
    end

    def most_recent_rule_application : ApplyCall?
      @current_apply_node.try(&.apply_call)
    end

    # returns an array of {pos, rule} pairs representing the ApplyCalls from the call stack that all resulted in left recursion
    def rule_in_recursion_call_stack_state : Array({Int32, Rule})
      apply_calls_that_resulted_in_left_recursion.map {|apply_call| {apply_call.pos, apply_call.rule} }
    end

    def top_apply_call_that_resulted_in_left_recursion : ApplyCallTree?
      if top_apply_node = @current_apply_node
        node : ApplyCallTree? = top_apply_node
        while node
          return node if node.apply_call.resulted_in_left_recursion?
          node = node.parent_apply_node
        end
        return nil
      end
    end


    # memoization

    def exist?(pos : Int32, rule : Rule) : Bool
      memoization_scope = top_apply_call_that_resulted_in_left_recursion || @root_memoization_scope
      memoization_scope.local_exist?(pos, rule)
    end

    def lookup(pos : Int32, rule : Rule) : MemoizedParseTree?
      memoization_scope = top_apply_call_that_resulted_in_left_recursion || @root_memoization_scope
      memo_table = memoization_scope.memo_table
      col = memo_table[pos]?
      col[rule]? if col
    end

    def add(pos : Int32, rule : Rule, memoized_parse_tree : MemoizedParseTree) : MemoizedParseTree
      memoization_scope = top_apply_call_that_resulted_in_left_recursion || @root_memoization_scope
      memoization_scope.local_memoize(pos, rule, memoized_parse_tree)
    end

  end
end