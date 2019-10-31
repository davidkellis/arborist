module Arborist
  class MemoResult
    property parse_tree : ParseTree?    # the parse tree matched at the index position within the memotable array at which this memoresult exists
    property next_pos : Int32

    def initialize(@parse_tree = nil, @next_pos = 0)
    end
  end

  # Column is a Map from rule_name => memoized-parse-tree-with-next-pos-state
  alias Column = Hash(String, MemoResult)

  class MemoTree
    # stores the memoized parse trees that occur in the rule-in-recursion call stack state represented by this MemoTree node
    property memo_table : Hash(Int32, Column)

    # stores memoization tables for ApplyCalls that occur in a more specific rule-in-recursion call stack state than is represented by this MemoTree node
    # The tuple representing the key is of the form: {pos, rule_name}
    property children : Hash({Int32, String}, MemoTree)

    def initialize()
      @memo_table = {} of Int32 => Column
      @children = {} of {Int32, String} => MemoTree
    end

    def clone()
      mt = MemoTree.new
      self.memo_table.each do |pos, col|
        mt.memo_table[pos] = col.dup    # shallow copy of Column
      end
      mt
    end

    def exist?(rule_in_recursion_call_stack_state : Array({Int32, String}), pos : Int32, rule_name : String) : Bool
      tree_node : MemoTree = self
      rule_in_recursion_call_stack_state.each do |pos_rule_name_pair|
        tree_node = tree_node.children[pos_rule_name_pair]? || (return false)
      end

      tree_node.local_exist?(pos, rule_name)
    end

    def local_exist?(pos : Int32, rule_name : String) : Bool
      col = self.memo_table[pos]?
      (col && col.has_key?(rule_name)) || false
    end

    def lookup(rule_in_recursion_call_stack_state : Array({Int32, String}), pos : Int32, rule_name : String) : MemoResult?
      tree_node : MemoTree = self
      rule_in_recursion_call_stack_state.each do |pos_rule_name_pair|
        tree_node = tree_node.children[pos_rule_name_pair]? || (return nil)
      end
      memo_table = tree_node.memo_table
      col = memo_table[pos]?
      col[rule_name]? if col


      # tree_node = lookup_tree_node(rule_in_recursion_call_stack_state)
      # if tree_node
      #   memo_table = tree_node.memo_table
      #   col = memo_table[pos]?
      #   col[rule_name]? if col
      # end


      # tree_node = self
      # memo_table = tree_node.memo_table
      # col = memo_table[pos]?
      # memo_result = col[rule_name]? if col
      # return memo_result if memo_result

      # rule_in_recursion_call_stack_state.each do |pos_rule_name_pair|
      #   if tree_node.children.has_key?(pos_rule_name_pair)
      #     tree_node = tree_node.children[pos_rule_name_pair]
      #     memo_table = tree_node.memo_table
      #     col = memo_table[pos]?
      #     memo_result = col[rule_name]? if col
      #     return memo_result if memo_result
      #   else
      #     return nil
      #   end
      # end
    end

    def local_lookup(pos : Int32, rule_name : String) : MemoResult
      self.memo_table[pos][rule_name]
    end


    def add(rule_in_recursion_call_stack_state : Array({Int32, String}), pos : Int32, rule_name : String, memo_result : MemoResult) : MemoResult
      tree_node = self
      rule_in_recursion_call_stack_state.each do |pos_rule_name_pair|
        tree_node = (tree_node.children[pos_rule_name_pair] ||= tree_node.clone)
      end

      tree_node.local_memoize(pos, rule_name, memo_result)
    end

    def local_memoize(pos : Int32, rule_name : String, memo_result : MemoResult) : MemoResult
      memo_table = self.memo_table
      col = (memo_table[pos] ||= Column.new)
      col[rule_name] = memo_result
    end

    private def lookup_tree_node(rule_in_recursion_call_stack_state : Array({Int32, String})) : MemoTree?
      tree_node = self
      rule_in_recursion_call_stack_state.each do |pos_rule_name_pair|
        child = tree_node.children[pos_rule_name_pair]?
        return nil unless child
        tree_node = child
      end
      tree_node
    end
  end


  class Matcher
    include DSL
    
    # @memoTable : Hash(Int32, Column)
    @memo_tree : MemoTree
    getter input : String
    property pos : Int32
    getter rules : Hash(String, Rule)
    property growing : Hash(Rule, Hash(Int32, ParseTree?))   # growing is a map <R -> <P -> seed >> from rules to maps of input positions to seeds at that input position. This is used to record the ongoing growth of a seed for a rule R at input position P.
    property expr_call_stack : Array(ExprCall)
    property fail_all_rules_until_this_rule : ApplyCall?
    property expr_failures : Hash(Int32, Set(Expr))

    def initialize(rules = {} of String => Rule)
      @rules = rules

      # these structures are necessary for handling left recursion
      @growing = {} of Rule => Hash(Int32, ParseTree?)
      @expr_call_stack = [] of ExprCall
      @fail_all_rules_until_this_rule = nil
      @expr_failures = {} of Int32 => Set(Expr)

      @input = ""
      # @memoTable = {} of Int32 => Column
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
    def match(input, start_rule_name = (@rules.first_key? || "start")) : ApplyTree?
      @input = input
      
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
      # @memoTable = {} of Int32 => Column
      @memo_tree = MemoTree.new

      @pos = 0

      add_skip_rule_if_necessary

      # the next 4 lines implement line 1 of Algorithm 2 from https://tratt.net/laurie/research/pubs/html/tratt__direct_left_recursive_parsing_expression_grammars/
      @growing = {} of Rule => Hash(Int32, ParseTree?)
      @rules.each_value do |rule|
        @growing[rule] = {} of Int32 => ParseTree?
      end

      @expr_call_stack = [] of ExprCall
      @fail_all_rules_until_this_rule = nil
      @expr_failures = {} of Int32 => Set(Expr)
    end

    def add_skip_rule_if_necessary
      skip_expr = (@skip_expr ||= MutexAlt.new( ('\u0000'..' ').map(&.to_s).to_set ) )
      add_rule("skip", skip_expr) unless @rules.has_key?("skip")
    end

    # returns the deepest/most-recent application of `rule` at position `pos` in the rule application stack
    def lookup_rule_application_in_call_stack(rule, pos) : ApplyCall?
      i = @expr_call_stack.size - 1
      while i >= 0
        expr_application_i = @expr_call_stack[i]
        i -= 1
        next unless expr_application_i.is_a?(ApplyCall)
        return expr_application_i if expr_application_i.rule == rule && expr_application_i.pos == pos
      end
      nil
    end

    # returns the deepest/most-recent left-recursive application of `rule` in the rule application stack
    def lookup_left_recursive_rule_application(rule) : ApplyCall?
      i = @expr_call_stack.size - 1
      while i >= 0
        expr_application_i = @expr_call_stack[i]
        i -= 1
        next unless expr_application_i.is_a?(ApplyCall)
        return expr_application_i if expr_application_i.rule == rule && expr_application_i.left_recursive?
      end
      nil
    end

    # returns the leftmost/earliest/oldest/shallowest application of `rule` in the rule application stack that resulted in left recursion
    def lookup_oldest_left_recursive_rule_application(rule) : ApplyCall?
      @expr_call_stack.each do |expr_application|
        next unless expr_application.is_a?(ApplyCall)
        return expr_application if expr_application.rule == rule && expr_application.resulted_in_left_recursion?
      end
      nil
    end

    # consults the rule application stack and returns the deepest/most-recent left-recursive 
    # application of any rule occuring at position `pos`
    def lookup_deepest_left_recursive_rule_appliation(pos) : ApplyCall?
      i = @expr_call_stack.size - 1
      while i >= 0
        expr_application_i = @expr_call_stack[i]
        i -= 1
        next unless expr_application_i.is_a?(ApplyCall)
        return expr_application_i if expr_application_i.left_recursive? && expr_application_id.pos == pos
      end
      nil
    end

    def log_match_failure(pos : Int32, expr : Expr) : Nil
      failures = (@expr_failures[pos] ||= Set(Expr).new)
      failures << expr
      nil
    end

    def print_match_failure_error
      pos = @expr_failures.keys.max
      if pos
        failed_exprs = @expr_failures[pos]
        start_pos = [pos - 10, 0].max
        puts "Malformed input fragment at position #{pos+1}:"
        puts @input[start_pos, 40]
        puts "#{"-" * 10}^"
        puts "Expected one of the following expressions to match at position #{pos+1}:"
        failed_exprs.each do |expr|
          puts expr.to_s
        end
      else
        puts "No match failures were logged."
      end
    end

    def fail_all_rules_back_to(previous_application_of_rule : ApplyCall)
      @fail_all_rules_until_this_rule = previous_application_of_rule
    end

    def fail_all_rules?
      !!@fail_all_rules_until_this_rule
    end

    def push_onto_call_stack(expr_application : ExprCall)
      @expr_call_stack.push(expr_application)
      expr_application
    end

    def pop_off_of_call_stack() : ExprCall
      @expr_call_stack.pop
    end

    def has_memoized_result?(rule_name) : Bool
      @memo_tree.exist?(rule_in_recursion_call_stack_state, @pos, rule_name)
      # col = @memoTable[@pos]?
      # !!col && col.has_key?(rule_name)
    end

    def memoize_result(pos, next_pos, rule_name, parse_tree : ParseTree?) : MemoResult
      GlobalDebug.puts "memoizing #{rule_name} at #{pos}-#{next_pos} with rule_in_recursion_call_stack_state=#{rule_in_recursion_call_stack_state} : '#{parse_tree.try(&.syntax_tree) || "nil"}'"
      @memo_tree.add(rule_in_recursion_call_stack_state, pos, rule_name, MemoResult.new(parse_tree, next_pos))
      # col = (@memoTable[pos] ||= {} of String => MemoResult)
      # col[rule_name] = MemoResult.new(parse_tree, next_pos)
    end

    def use_memoized_result(rule_name) : ParseTree?
      memo_result = @memo_tree.lookup(rule_in_recursion_call_stack_state, @pos, rule_name)
      if memo_result
        @pos = memo_result.next_pos
        memo_result.parse_tree
      end
      # col = @memoTable[@pos]
      # memo_result = col[rule_name]
      # @pos = memo_result.next_pos
      # memo_result.parse_tree
    end

    # this method marks all ApplyCall calls on the call stack occurring more recent than oldest_application as unsafe to memoize
    def mark_most_recent_applications_unsafe_to_memoize(oldest_application)
      apply_calls = apply_calls_in_call_stack
      i = apply_calls.index(oldest_application)
      apply_calls[i...].skip(1).each(&.not_safe_to_memoize!)
    end

    def left_recursive_apply_calls : Array(ApplyCall)
      @expr_call_stack.select {|expr_call| expr_call.is_a?(ApplyCall) && expr_call.left_recursive? }.map(&.as(ApplyCall))
    end

    def apply_calls_that_resulted_in_left_recursion : Array(ApplyCall)
      @expr_call_stack.select {|expr_call| expr_call.is_a?(ApplyCall) && expr_call.resulted_in_left_recursion? }.map(&.as(ApplyCall))
    end

    def apply_calls_in_call_stack : Array(ApplyCall)
      @expr_call_stack.select {|expr_call| expr_call.is_a?(ApplyCall) }.map(&.as(ApplyCall))
    end

    # returns an array of pairs of the form {pos, rule_name}, each summarizing an ApplyCall
    def rule_in_recursion_call_stack_state : Array({Int32, String})
      apply_calls_that_resulted_in_left_recursion.map {|apply_call| {apply_call.pos, apply_call.rule_name} }
    end

    # def any_left_recursion_ongoing? : Bool
    #   apply_calls = @expr_call_stack.select {|expr_call| expr_call.is_a?(ApplyCall) }.map(&.as(ApplyCall))
    #   apply_calls.any?(&.left_recursive?)
    # end

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

    def add_rule_if_necessary(rule_name : String, expr : Expr)
      add_rule(rule_name, expr) unless @rules.has_key?(rule_name)
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
      add_rule_if_necessary(skip_rule_name, skip_rule_expr)
      
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
      i = @expr_call_stack.size - 1
      while i >= 0
        expr_application_i = @expr_call_stack[i]
        i -= 1
        next unless expr_application_i.is_a?(ApplyCall)
        return expr_application_i
      end
      nil
    end
  end
end