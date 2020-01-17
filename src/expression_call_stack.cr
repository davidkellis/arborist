module Arborist
  class ExpressionCallStack
    @call_stack : Array(ExprCall)
    @apply_call_stack : Array(ApplyCall)
    @apply_call_stacks_per_rule : Hash(Rule, Array(ApplyCall))
    @apply_calls_per_rule_and_pos : Hash(Tuple(Rule, Int32), Array(ApplyCall))
    
    def initialize
      @call_stack = [] of ExprCall
      @apply_call_stack = [] of ApplyCall   # this is a subset of things added to @call_stack
      @apply_call_stacks_per_rule = Hash(Rule, Array(ApplyCall)).new
      @apply_calls_per_rule_and_pos = Hash(Tuple(Rule, Int32), Array(ApplyCall)).new
    end

    def reset
      @call_stack = [] of ExprCall
      @apply_call_stack = [] of ApplyCall   # this is a subset of things added to @call_stack
      @apply_call_stacks_per_rule = Hash(Rule, Array(ApplyCall)).new
      @apply_calls_per_rule_and_pos = Hash(Tuple(Rule, Int32), Array(ApplyCall)).new
    end

    def apply_calls_in_call_stack : Array(ApplyCall)
      # @expr_call_stack.select {|expr_call| expr_call.is_a?(ApplyCall) }.map(&.as(ApplyCall))
      @apply_call_stack
    end

    def apply_calls_that_resulted_in_left_recursion : Array(ApplyCall)
      # @expr_call_stack.select {|expr_call| expr_call.is_a?(ApplyCall) && expr_call.resulted_in_left_recursion? }.map(&.as(ApplyCall))
      apply_calls_in_call_stack.select(&.resulted_in_left_recursion?)
    end

    def left_recursive_apply_calls : Array(ApplyCall)
      # @expr_call_stack.select {|expr_call| expr_call.is_a?(ApplyCall) && expr_call.left_recursive? }.map(&.as(ApplyCall))
      apply_calls_in_call_stack.select(&.left_recursive?)
    end

    def lookup_rule_application_in_call_stack(rule : Rule, pos : Int32) : ApplyCall?
      # i = @expr_call_stack.size - 1
      # while i >= 0
      #   expr_application_i = @expr_call_stack[i]
      #   i -= 1
      #   next unless expr_application_i.is_a?(ApplyCall)
      #   return expr_application_i if expr_application_i.rule == rule && expr_application_i.pos == pos
      # end
      # nil
      @apply_calls_per_rule_and_pos[{rule, pos}]?.try(&.last?)
    end

    # returns the deepest/most-recent application of `rule` in the rule application stack
    def lookup_most_recent_rule_application_in_call_stack(rule : Rule) : ApplyCall?
      # i = @expr_call_stack.size - 1
      # while i >= 0
      #   expr_application_i = @expr_call_stack[i]
      #   i -= 1
      #   next unless expr_application_i.is_a?(ApplyCall)
      #   return expr_application_i if expr_application_i.rule == rule
      # end
      # nil

      apply_call_stack_for_rule = @apply_call_stacks_per_rule[rule]?
      apply_call_stack_for_rule.last? if apply_call_stack_for_rule
    end

    # returns the leftmost/earliest/oldest/shallowest application of `rule` in the rule application stack that resulted in left recursion
    def lookup_oldest_rule_application_that_resulted_in_left_recursion(rule) : ApplyCall?
      # @expr_call_stack.each do |expr_application|
      #   next unless expr_application.is_a?(ApplyCall)
      #   return expr_application if expr_application.rule == rule && expr_application.resulted_in_left_recursion?
      # end
      # nil
      apply_calls_that_resulted_in_left_recursion.find {|apply_call| apply_call.rule == rule }
    end

    def most_recent_rule_application : ApplyCall?
      # i = @expr_call_stack.size - 1
      # while i >= 0
      #   expr_application_i = @expr_call_stack[i]
      #   i -= 1
      #   next unless expr_application_i.is_a?(ApplyCall)
      #   return expr_application_i
      # end
      # nil
      apply_calls_in_call_stack.last?
    end

    def pop_off_of_call_stack(the_top_of_stack_expr_call_successfully_parsed : Bool) : ExprCall
      # seed_growth_controller.current_expr_call_failed unless the_top_of_stack_expr_call_successfully_parsed
      # seed_growth_controller.pop_off_of_call_stack()
      popped_expr_call = @call_stack.pop
      if popped_expr_call.is_a?(ApplyCall)
        @apply_call_stack.pop
        @apply_call_stacks_per_rule[popped_expr_call.rule].pop()
        @apply_calls_per_rule_and_pos[{popped_expr_call.rule, popped_expr_call.pos}].pop()
      end
      popped_expr_call
    end

    def push_onto_call_stack(expr_call : ExprCall)
      @call_stack.push(expr_call)
      if expr_call.is_a?(ApplyCall)
        @apply_call_stack.push(expr_call)

        apply_call_stack_for_rule = (@apply_call_stacks_per_rule[expr_call.rule] ||= [] of ApplyCall)
        apply_call_stack_for_rule.push(expr_call)

        apply_call_stack_for_rule_and_pos = (@apply_calls_per_rule_and_pos[{expr_call.rule, expr_call.pos}] ||= [] of ApplyCall)
        apply_call_stack_for_rule_and_pos.push(expr_call)
      end
      # seed_growth_controller.push_onto_call_stack(expr_call)
      expr_call
    end

    # returns an array of pairs of the form {pos, rule_name}, each summarizing an ApplyCall
    def rule_in_recursion_call_stack_state : Array({Int32, String})
      apply_calls_that_resulted_in_left_recursion.map {|apply_call| {apply_call.pos, apply_call.rule_name} }
    end

  end
end