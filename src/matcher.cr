require "./dsl"
require "./char_array"
require "./expression_call_stack"
require "./memoization"

module Arborist
  class MatchFailure
    property pos : Int32
    property expr : Expr
    property active_rule_name : String
    property apply_calls_in_call_stack : Array(ApplyCall)

    def initialize(@pos, @expr, @apply_calls_in_call_stack)
      @active_rule_name = @apply_calls_in_call_stack.last.rule_name
    end

    def hash
      pos.hash ^ expr.hash ^ active_rule_name.hash
    end

    def ==(other)
      pos == other.pos &&
        expr == other.expr &&
        active_rule_name == other.active_rule_name
    end
  end


  class Matcher
    include DSL
    
    @memo_tree : MemoTree
    getter input : CharArray
    property pos : Int32
    getter rules : Hash(String, Rule)
    property growing : Hash(Rule, Hash(Int32, ParseTree?))   # growing is a map <R -> <P -> seed >> from rules to maps of input positions to seeds at that input position. This is used to record the ongoing growth of a seed for a rule R at input position P.
    property expr_call_stack : ExpressionCallStack
    property expr_failures : Hash(Int32, Set(MatchFailure))
    property seed_growth_controller : SeedGrowthController

    def initialize(rules = {} of String => Rule)
      @rules = rules

      # these structures are necessary for handling left recursion
      @growing = {} of Rule => Hash(Int32, ParseTree?)
      # @expr_call_stack = [] of ExprCall
      @expr_call_stack = ExpressionCallStack.new
      @expr_failures = {} of Int32 => Set(MatchFailure)
      @seed_growth_controller = SeedGrowthController.new

      @input = CharArray.new("")
      @memo_tree = MemoTree.new
      @pos = 0
    end
    
    def call_stack
      expr_call_stack
    end

    def call_stack_apply_calls
      expr_call_stack.select(&.is_a?(ApplyCall))
    end

    def add_rule(rule_name, expr : Expr)
      @rules[rule_name] = Rule.new(self, rule_name, expr)
      self
    end

    def get_rule(rule_name) : Rule
      @rules[rule_name]
    end

    # returns nil if the grammar rules don't match the full input string
    def match(input : String, start_rule_name = (@rules.first_key? || "start")) : ApplyTree?
      @input = CharArray.new(input)
      
      prepare_for_matching    # (re)initialize the growing map and limit set just prior to use

      start_expr = Apply.new(start_rule_name)
      apply_parse_tree = start_expr.eval(self).as(ApplyTree?)
      if apply_parse_tree
        apply_parse_tree.recursively_populate_parents
        apply_parse_tree if @pos == @input.size
      end
    end

    # per https://tratt.net/laurie/research/pubs/html/tratt__direct_left_recursive_parsing_expression_grammars/:
    # growing is the data structure at the heart of the algorithm. 
    # A programming language-like type for it would be Map<Rule,Map<Int,Result>>. 
    # Since we statically know all the rules for a PEG, growing is statically initialised with an 
    # empty map for each rule at the beginning of the algorithm (line 1).
    #
    # So, we want to initialize the growing map just prior to using it, since that will be the only point that we know for sure that
    # all of the rules have been added to the matcher.
    def prepare_for_matching
      @memo_tree = MemoTree.new

      @pos = 0

      add_skip_rule_if_necessary

      # the next 4 lines implement line 1 of Algorithm 2 from https://tratt.net/laurie/research/pubs/html/tratt__direct_left_recursive_parsing_expression_grammars/
      @growing = {} of Rule => Hash(Int32, ParseTree?)
      @rules.each_value do |rule|
        initialize_seed_growing_hash_for_rule(rule)
      end

      # @expr_call_stack = [] of ExprCall
      @expr_call_stack.reset
      @expr_failures = {} of Int32 => Set(MatchFailure)
      @seed_growth_controller.reset
    end

    def initialize_seed_growing_hash_for_rule(rule)
      @growing[rule] = {} of Int32 => ParseTree?
    end

    def add_skip_rule_if_necessary
      skip_expr = (@skip_expr ||= MutexAlt.new( ('\u0000'..' ').map(&.to_s).to_set ) )
      add_rule("skip", skip_expr) unless @rules.has_key?("skip")
    end

    # returns the deepest/most-recent application of `rule` at position `pos` in the rule application stack
    def lookup_rule_application_in_call_stack(rule, pos) : ApplyCall?
      # i = @expr_call_stack.size - 1
      # while i >= 0
      #   expr_application_i = @expr_call_stack[i]
      #   i -= 1
      #   next unless expr_application_i.is_a?(ApplyCall)
      #   return expr_application_i if expr_application_i.rule == rule && expr_application_i.pos == pos
      # end
      # nil
      @expr_call_stack.lookup_rule_application_in_call_stack(rule, pos)
    end

    # returns the deepest/most-recent application of `rule` in the rule application stack
    def lookup_most_recent_rule_application_in_call_stack(rule) : ApplyCall?
      # i = @expr_call_stack.size - 1
      # while i >= 0
      #   expr_application_i = @expr_call_stack[i]
      #   i -= 1
      #   next unless expr_application_i.is_a?(ApplyCall)
      #   return expr_application_i if expr_application_i.rule == rule
      # end
      # nil
      @expr_call_stack.lookup_most_recent_rule_application_in_call_stack(rule)
    end

    # returns the leftmost/earliest/oldest/shallowest application of `rule` in the rule application stack that resulted in left recursion
    def lookup_oldest_rule_application_that_resulted_in_left_recursion(rule) : ApplyCall?
      # @expr_call_stack.each do |expr_application|
      #   next unless expr_application.is_a?(ApplyCall)
      #   return expr_application if expr_application.rule == rule && expr_application.resulted_in_left_recursion?
      # end
      # nil
      @expr_call_stack.lookup_oldest_rule_application_that_resulted_in_left_recursion(rule)
    end

    def log_match_failure(pos : Int32, expr : Expr) : Nil
      match_failure = MatchFailure.new(pos, expr, apply_calls_in_call_stack)
      failures = (@expr_failures[pos] ||= Set(MatchFailure).new)
      failures << match_failure
      nil
    end

    def print_match_failure_error
      pos = @expr_failures.keys.max
      if pos
        match_failures = @expr_failures[pos]

        input_length_up_to_and_including_error_char = pos + 1
        input_up_to_error = @input[0, input_length_up_to_and_including_error_char]
        lines_up_to_error = input_up_to_error.lines(false)
        error_position_line_number = lines_up_to_error.size
        lines_up_to_error.pop   # drop the last line
        character_total_prior_to_error_line = lines_up_to_error.reduce(0) do |memo, line|
          memo + line.each_char.size
        end
        error_position_on_line = input_length_up_to_and_including_error_char - character_total_prior_to_error_line
        puts "Malformed input fragment on line #{error_position_line_number} char #{error_position_on_line} ; at position #{pos+1} (character index #{pos}):"
        

        context_length = 40           # this should be an even number
        context_half_length = context_length // 2
        start_pos = [pos - context_half_length, 0].max
        length_of_context_up_to_and_including_error = pos - start_pos + 1
        error_context = @input[start_pos, context_length]

        char_count = 0
        need_to_print_error_indicator_line = true
        error_context.lines(false).each do |line|
          character_total_prior_to_error_line = char_count
          print line
          puts("") if line[-1] != '\n'
          char_count += line.size

          if need_to_print_error_indicator_line && char_count >= length_of_context_up_to_and_including_error
            need_to_print_error_indicator_line = false

            error_position_on_error_line = length_of_context_up_to_and_including_error - character_total_prior_to_error_line - 1
            puts "#{"-" * error_position_on_error_line}^"
          end
        end

        puts "Expected one of the following expressions to match on line #{error_position_line_number} char #{error_position_on_line} ; at position #{pos+1} (character index #{pos}):"
        match_failures.group_by(&.active_rule_name).each do |active_rule_name, match_failures_for_rule|
          puts active_rule_name
          match_failures_for_rule.each_with_index do |match_failure, index|
            puts "  #{index + 1}. #{match_failure.expr.to_s}"
            space_prefix = "  #{" " * (index + 1).to_s.size}  "
            apply_call_tree = match_failure.apply_calls_in_call_stack.map_with_index do |apply_call, apply_call_index|
              "#{space_prefix}#{apply_call.pos} - #{apply_call.rule_name}"
              # "#{space_prefix}#{"  " * apply_call_index}#{apply_call.pos} - #{apply_call.rule_name}"
            end.join("\n")
            puts apply_call_tree
          end
        end
      else
        puts "No match failures were logged."
      end
    end

    def push_onto_call_stack(expr_application : ExprCall)
      # @expr_call_stack.push(expr_application)
      @expr_call_stack.push_onto_call_stack(expr_application)
      seed_growth_controller.push_onto_call_stack(expr_application)
      expr_application
    end

    def pop_off_of_call_stack(the_top_of_stack_expr_call_successfully_parsed : Bool) : ExprCall
      seed_growth_controller.current_expr_call_failed unless the_top_of_stack_expr_call_successfully_parsed
      seed_growth_controller.pop_off_of_call_stack()
      # @expr_call_stack.pop
      @expr_call_stack.pop_off_of_call_stack(the_top_of_stack_expr_call_successfully_parsed)
    end

    def has_memoized_result?(rule : Rule) : Bool
      @memo_tree.exist?(rule_in_recursion_call_stack_state, @pos, rule)
    end

    def memoize_result(pos, next_pos, rule, parse_tree : ParseTree?) : MemoResult
      GlobalDebug.puts "memoizing #{rule.name} at #{pos}-#{next_pos} with rule_in_recursion_call_stack_state=#{rule_in_recursion_call_stack_state} : '#{parse_tree.try(&.syntax_tree) || "nil"}'"
      @memo_tree.add(rule_in_recursion_call_stack_state, pos, rule, MemoResult.new(parse_tree, next_pos))
    end

    def use_memoized_result(rule_name) : ParseTree?
      memo_result = @memo_tree.lookup(rule_in_recursion_call_stack_state, @pos, rule_name)
      if memo_result
        @pos = memo_result.next_pos
        memo_result.parse_tree
      end
    end

    # this method marks all ApplyCall calls on the call stack occurring more recent than oldest_application as unsafe to memoize
    def mark_most_recent_applications_unsafe_to_memoize(oldest_application)
      apply_calls = apply_calls_in_call_stack
      i = apply_calls.index(oldest_application)
      apply_calls[i...].skip(1).each(&.not_safe_to_memoize!)
    end

    def left_recursive_apply_calls : Array(ApplyCall)
      # @expr_call_stack.select {|expr_call| expr_call.is_a?(ApplyCall) && expr_call.left_recursive? }.map(&.as(ApplyCall))
      @expr_call_stack.left_recursive_apply_calls
    end

    def apply_calls_that_resulted_in_left_recursion : Array(ApplyCall)
      # @expr_call_stack.select {|expr_call| expr_call.is_a?(ApplyCall) && expr_call.resulted_in_left_recursion? }.map(&.as(ApplyCall))
      @expr_call_stack.apply_calls_that_resulted_in_left_recursion
    end

    def apply_calls_in_call_stack : Array(ApplyCall)
      # @expr_call_stack.select {|expr_call| expr_call.is_a?(ApplyCall) }.map(&.as(ApplyCall))
      @expr_call_stack.apply_calls_in_call_stack
    end

    # returns an array of pairs of the form {pos, rule}, each summarizing an ApplyCall
    def rule_in_recursion_call_stack_state : Array({Int32, Rule})
      # apply_calls_that_resulted_in_left_recursion.map {|apply_call| {apply_call.pos, apply_call.rule_name} }
      @expr_call_stack.rule_in_recursion_call_stack_state
    end

    def eof?
      @pos >= @input.size
    end

    def consume(c : Char) : Bool
      if @input[@pos] == c
        @pos += 1
        true
      else
        false
      end
    end

    # consumes a string of length `count`
    # returns nil if unable to consume `count` characters
    def consume(count : Int32) : String?
      remaining_chars_in_input = @input.size - @pos
      return nil if count > remaining_chars_in_input
      
      str = @input[@pos, count]
      @pos += count
      str
    end

    def add_rule_while_matching_is_in_progress_if_necessary(rule_name : String, expr : Expr)
      unless @rules.has_key?(rule_name)
        add_rule(rule_name, expr)
        new_rule = get_rule(rule_name)
        initialize_seed_growing_hash_for_rule(new_rule)
      end
    end

    # The application of the skip rule is internally represented as a parameterized rule, such that the skip rule is specialized
    # for each expression that will follow. This approach made it possible implement the skip rule as:
    # parameterized_skip_rule[following_expr] <- !following_expr skip*
    # Eventually I concluded that implementing the skip rule as `!{following expression} skip*` was not what I wanted right now,
    # but this establishes the pattern of implementing parameterized rules, and I may decide to go back to implementing the skip
    # rule as `!{following expression} skip*`.
    def apply_skip_rule(expr : Expr)
      skip_rule_name = "__skip_prior_to_expr_#{expr.object_id}"

      # skip_rule_expr = seq(star(apply("skip")), pos(expr))
      
      skip_rule_expr = star(apply("skip"))
      add_rule_while_matching_is_in_progress_if_necessary(skip_rule_name, skip_rule_expr)
      
      apply_skip = apply(skip_rule_name)
      # puts "skip start - #{"+" * 100}"
      retval = apply_skip.eval(self)
      # puts "skip end - matched #{retval.try(&.text.size)} chars - #{"-" * 100}"
      retval
    end

    def skip_whitespace_if_in_syntactic_context(expr : Expr)
      rule_application = most_recent_rule_application
      apply_skip_rule(expr) if rule_application && rule_application.syntactic_rule?
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
      @expr_call_stack.most_recent_rule_application
    end
  end
end