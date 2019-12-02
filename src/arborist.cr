# Based on https://github.com/ohmlang/sle17/blob/master/src/standard.js
# https://ohmlang.github.io/pubs/sle2017/incremental-packrat-parsing.pdf
# https://tratt.net/laurie/research/pubs/html/tratt__direct_left_recursive_parsing_expression_grammars/
# http://www.vpri.org/pdf/tr2007002_packrat.pdf

# Per http://bford.info/pub/lang/peg.pdf:
# Definition:
# A parsing expression grammar (PEG) is a 4-tuple G = (VN, VT, R, eS),
# where VN is a finite set of nonterminal symbols, 
# VT is a finite set of terminal symbols, 
# R is a finite set of rules, 
# eS is a parsing expression termed the start expression, and 
# VN intersection VT = empty set.
# Each rule r ∈ R is a pair (A,e), which we write A <- e, where
# A ∈ VN and e is a parsing expression. For any nonterminal A, there
# is exactly one e such that (A <- e) ∈ R. R is therefore a function
# from nonterminals to expressions, and we write R(A) to denote the
# unique expression e such that (A <- e) ∈ R.

require "./grammar"
require "./matcher"
require "./parse_tree"
require "./visitor"

module Arborist
  class GlobalDebug
    @@enabled : Bool = false
    @@indent : Int32 = -1
    def self.enabled?
      @@enabled
    end
    def self.enable(enable : Bool)
      @@enabled = enable
    end
    def self.enable!
      enable(true)
    end
    def self.disable!
      enable(false)
    end
    def self.indent
      @@indent
    end
    def self.prefix(prefix_str : String)
      prefix_str * @@indent
    end
    def self.increment
      @@indent += 1
    end
    def self.decrement
      @@indent -= 1
    end
    def self.reset_indent
      @@indent = -1
    end
    def self.puts(str)
      STDOUT.puts("#{prefix("|  ")}#{str}") if enabled?
    end
  end

  class Rule
    getter matcher : Matcher
    property name : String
    property expr : Expr

    def initialize(@matcher, @name, @expr)
    end

    def to_s
      "#{@name} -> #{@expr.to_s}"
    end
  end

  # The various ExprCall classes represent invocations, or calls, of the various expression types, at different positions
  # in an input string. The invocations/calls form a call stack, because a PEG parser is by nature a recursive descent parser,
  # and each rule application and the evaluations of the different expressions that make up those rules form a call stack.
  class ExprCall
    property expr : Expr
    property pos : Int32

    def initialize(@expr, @pos)
    end

    def inspect(io)
      io.print("#{self.class.name}:#{self.object_id} at #{@pos}: #{@expr.class.name} #{@expr.to_s}")
    end
  end

  class TerminalCall < ExprCall
  end
  class MutexAltCall < ExprCall
  end
  class ChoiceCall < ExprCall
  end
  class SequenceCall < ExprCall
  end
  class NegLookAheadCall < ExprCall
  end
  class PosLookAheadCall < ExprCall
  end
  class OptionalCall < ExprCall
  end
  class RepetitionCall < ExprCall
  end
  class RepetitionOnePlusCall < ExprCall
  end

  class ApplyCall < ExprCall
    # property expr : Expr   # inherited from ExprCall
    # property pos : Int32   # inherited from ExprCall
    getter rule : Rule
    property left_recursive : Bool
    property seed_parse_tree : ParseTree?
    property resulted_in_left_recursion : Bool
    property safe_to_memoize : Bool

    def initialize(apply_expr : Apply, @rule, @pos, @left_recursive = false)
      @expr = apply_expr
      @seed_parse_tree = nil
      @resulted_in_left_recursion = false
      @safe_to_memoize = true
    end

    def rule_name
      @rule.name
    end

    def inspect(io)
      io.print("#{self.class.name}:#{self.object_id} at #{@pos}: #{@rule.name} -> #{@rule.expr.to_s}")
    end

    # Sets the `seed_parse_tree` field to the longest match parse tree between the existing value
    # of the `seed_parse_tree` field and the supplied `candidate_seed_parse_tree`.
    # Returns the longest candidate seed parse tree observed (including `candidate_seed_parse_tree`)
    def add_candidate_seed_parse_tree(candidate_seed_parse_tree)
      existing_seed_parse_tree = self.seed_parse_tree
      if candidate_seed_parse_tree
        if existing_seed_parse_tree
          if candidate_seed_parse_tree.text.size > existing_seed_parse_tree.text.size   # todo: should I use ParseTree#finishing_pos here to compare instead of text.size ? Probably so.
            self.seed_parse_tree = candidate_seed_parse_tree
          else
            existing_seed_parse_tree
          end
        else
          self.seed_parse_tree = candidate_seed_parse_tree
        end
      else
        existing_seed_parse_tree
      end
    end

    # returns true if this rule application is left recursive at `@pos`; false otherwise
    def left_recursive?
      @left_recursive
    end

    def set_left_recursive(new_value)
      @left_recursive = new_value
    end

    def resulted_in_left_recursion?
      @resulted_in_left_recursion
    end

    def set_resulted_in_left_recursion(new_value)
      @resulted_in_left_recursion = new_value
    end

    def safe_to_memoize?
      @safe_to_memoize
    end

    def not_safe_to_memoize!
      @safe_to_memoize = false
    end

    def syntactic_rule?
      @expr.as(Apply).syntactic_rule?
    end
  end



  alias Expr = Apply | Terminal | MutexAlt | Dot | Choice | Sequence | NegLookAhead | PosLookAhead | Optional | Repetition | RepetitionOnePlus

  # Apply represents the application of a named rule
  class Apply
    GlobalDebug.reset_indent
    getter rule_name : String
    property label : String?

    def initialize(rule_name)
      @rule_name = rule_name
    end

    def label(label : String) : Apply
      @label = label
      self
    end

    def syntactic_rule?
      first_char = @rule_name[0]?
      first_char.try(&.uppercase?)
    end

    def preorder_traverse(matcher, visit : Expr -> _, visited_nodes : Set(Expr))
      return if visited_nodes.includes?(self)
      visited_nodes << self
      visit.call(self)
      rule = matcher.get_rule(@rule_name)
      rule.expr.preorder_traverse(matcher, visit, visited_nodes)
    end

    # this implements Tratt's Algorithm 2 in section 6.4 of https://tratt.net/laurie/research/pubs/html/tratt__direct_left_recursive_parsing_expression_grammars/
    def eval(matcher) : ParseTree?   # line 3 of Algorithm 2
      GlobalDebug.increment
      # return (GlobalDebug.decrement; nil) if matcher.fail_all_rules?

      rule = matcher.get_rule(@rule_name)
      pos = matcher.pos
      GlobalDebug.puts "try apply #{@rule_name} at #{pos}"

      top_level_return_parse_tree = if matcher.has_memoized_result?(@rule_name)
        memoized_apply_tree = matcher.use_memoized_result(@rule_name)
        # we are overriding whatever label is attached to the memoized ApplyTree node, because it's possible that the memoized ApplyTree
        # was created with a label that is associated with a different invocation/application of the Rule identified by @rule_name than the 
        # invocation/application that this Apply object represents.
        memoized_apply_tree.set_label(@label) if memoized_apply_tree
        GlobalDebug.puts "found memoized apply #{@rule_name} at #{pos} : #{memoized_apply_tree && memoized_apply_tree.as(ApplyTree).syntax_tree || "nil"}"
        memoized_apply_tree
      else
        # has this same rule been applied at the same position previously?
        previous_application_of_rule_at_pos = matcher.lookup_rule_application_in_call_stack(rule, pos)
        
        # if previous_application_of_rule_at_pos != nil, then a previous application of rule `rule` was attempted at position `pos`, 
        # so this application - once it is performed via a call to #simple_rule_application - is going to be a left recursive 
        # application of the same rule
        is_this_application_left_recursive_at_pos = !!previous_application_of_rule_at_pos

        previous_application_of_rule_at_pos.set_resulted_in_left_recursion(true) if previous_application_of_rule_at_pos

        # # look up the most recent left-recursive application of this rule, occurring at any position.
        # # if one is found, then that implies we are already growing a seed for this rule at some position
        # previous_left_recursive_application_of_rule = matcher.lookup_left_recursive_rule_application(rule)

        # # is this rule currently in left recursion anywhere on the application call stack?
        # is_rule_in_left_recursion_anywhere = !!previous_left_recursive_application_of_rule

        # # if we're already in left recursion on `rule` and we have a seed growing for `rule` at `pos`, then we are in left recursion on rule `rule` at position `pos`
        # # the name of this identifier is not quite right, but I can't figure out a good name for it - it indicates
        # # that the current application of this rule is 3-calls deep at the current position, because the second call was
        # # the first left recursive call, which would have started a seed growth, and this third call should return the seed
        # growing_seed_for_rule_at_current_position = is_rule_in_left_recursion_anywhere && matcher.growing[rule].has_key?(pos)

        current_rule_application = push_rule_application(matcher, rule, pos, is_this_application_left_recursive_at_pos)
        GlobalDebug.puts "push apply #{@rule_name} (call #{current_rule_application.object_id}) at #{pos} #{is_this_application_left_recursive_at_pos && "; LR" || ""}"

        parse_tree_from_rule_expr = if is_this_application_left_recursive_at_pos
          # mark all applications between this application and the parent application (excluding the parent) as not safe to memoize
          matcher.mark_most_recent_applications_unsafe_to_memoize(previous_application_of_rule_at_pos)

          # return seed
          seed_parse_tree = matcher.growing[rule][pos]
          if seed_parse_tree
            matcher.pos = seed_parse_tree.finishing_pos + 1
          else
            matcher.pos = pos
          end
          GlobalDebug.puts "return seed growth for #{@rule_name} at #{pos} : '#{seed_parse_tree.try(&.syntax_tree) || "nil"}'"
          seed_parse_tree
        else
          # This branch is all about growing the seed from the bottom up with Warth-style bottom-up seed-growing

          # We want to maximally grow the seed at the first occurrence of a left-recursive rule application of `rule`.
          # We want to minimally grow the seeds of all other left-recursive rule application of `rule` that occur while we are still
          # growing the seed associated with the first occurrence of a left-recursive rule application of `rule`.
          # This means that the first occurrence of left recursive seed growth for `rule` should continue until it can't grow anymore, but all
          # other occurrences of left recursive seed growth for `rule` should only grow one level and then fail.
          #
          # The rule of seed growth is:
          # Only the top-level seed growth for a given rule may allow left recursive calls; deeper-level seed growth on the
          # same rule may not be made up of any left-recursive calls.

          # Here, we are starting to grow a seed on a potentially left-recursive call.
          # We can only grow a single seed for a given rule at a time. All other would-be seed-growths for the same rule must fail,
          # in order to ensure the left-most seed growth consumes as much as possible.
          # Therefore, if we detect that this recursive call is a top-level seed growth for `rule`, then we want to grow the seed
          # as much as we can; otherwise, we conclude that this is not a top-level seed growth, and therefore needs to fail - i.e.
          # the non-top-level seed will start as nil, but will never be updated and the first parse tree returned from the first-level, 
          # second-level, third-level, etc.-level left-recursive call will be treated as the parse tree for those left-recursive
          # calls.

          matcher.growing[rule][pos] = nil
          full_grown_seed_to_return = nil
          current_rule_application.set_resulted_in_left_recursion(false)    # this may not even be necessary since the initial value is false

          GlobalDebug.puts "starting seed growth for rule #{rule.name} at #{pos}"
          while true
            matcher.pos = pos
            parse_tree = simple_rule_application(matcher, current_rule_application)

            oldest_application_resulting_in_left_recursion = matcher.lookup_oldest_left_recursive_rule_application(rule)
            if current_rule_application.resulted_in_left_recursion? &&                           # we only want to grow the seed bottom up if this application spawned a recursive apply call
                  oldest_application_resulting_in_left_recursion == current_rule_application     # and we only want to grow the seed on the leftmost/earliest/shallowest application in a call stack that resulted in left recursion on this rule; the subsequent applications of this rule that would have resulted in left recursion should not grow their seed (should not left-recurse)
              seed_parse_tree = matcher.growing[rule][pos]
              GlobalDebug.puts "candidate seed for #{rule.name} at #{pos} : parse_tree = '#{parse_tree.try(&.syntax_tree) || "nil"}' ; seed_parse_tree = '#{seed_parse_tree.try(&.syntax_tree) || "nil"}'"
              if parse_tree.nil? || (seed_parse_tree && parse_tree.finishing_pos <= seed_parse_tree.finishing_pos)   # we're done growing the seed; it can't grow any further
                GlobalDebug.puts "finished seed growth for #{rule.name} at #{pos}"
                full_grown_seed_to_return = seed_parse_tree
                if seed_parse_tree
                  matcher.pos = seed_parse_tree.finishing_pos + 1
                else
                  matcher.pos = pos
                end
                break
              end
            else
              full_grown_seed_to_return = parse_tree
              break
            end

            GlobalDebug.puts "grow seed #{rule.name} at #{pos} : '#{parse_tree.try(&.syntax_tree) || "nil"}'"
            matcher.growing[rule][pos] = parse_tree                         # line 25 of Algorithm 2
          end

          matcher.growing[rule].delete(pos)
          GlobalDebug.puts "finishing seed growth for rule #{rule.name} at #{pos} : '#{full_grown_seed_to_return.try(&.syntax_tree) || "nil"}'"
          full_grown_seed_to_return
        end                                                                 # line 35 of Algorithm 2

        popped_apply_call = pop_rule_application(matcher)

        apply_parse_tree = if parse_tree_from_rule_expr
          ApplyTree.new(parse_tree_from_rule_expr, @rule_name, matcher.input, pos, parse_tree_from_rule_expr.finishing_pos).label(@label)
        end

        if apply_parse_tree
          GlobalDebug.puts "matched #{@rule_name} (call #{popped_apply_call.object_id}) at #{pos} : '#{apply_parse_tree.syntax_tree}'"
        else
          GlobalDebug.puts "failed #{@rule_name} (call #{popped_apply_call.object_id}) at #{pos}"
        end

        # the aggregate state of the rules on the call stack that are in currently in recursion constitute a memoization context - parse
        # trees resulting from rule applications within the same context may be memoized and read from the memoization cache, but once
        # the context changes, then any reads must be taken from the part of the cache that corresponds to the rule-in-recursion stack state
        # at which the rule application is being made.
        matcher.memoize_result(pos, matcher.pos, @rule_name, apply_parse_tree) if popped_apply_call.safe_to_memoize?

        apply_parse_tree
      end

      GlobalDebug.decrement
      top_level_return_parse_tree
    end                                                                   # line 36 of Algorithm 2

    def simple_rule_application(matcher, current_rule_application) : ParseTree?
      # this logic captures "normal" rule application - no memoization, can't handle left recursion
      rule = matcher.rules[@rule_name]
      rule.expr.eval(matcher)
    end

    def push_rule_application(matcher, rule, pos, is_this_application_left_recursive_at_pos)
      current_rule_application = ApplyCall.new(self, rule, pos, is_this_application_left_recursive_at_pos)
      matcher.push_onto_call_stack(current_rule_application)
      current_rule_application
    end

    def pop_rule_application(matcher) : ApplyCall
      expr_call = matcher.pop_off_of_call_stack
      expr_call.is_a?(ApplyCall) ? expr_call : raise "unexpected ExprCall on call stack. expected an ApplyCall."
    end

    def to_s
      "apply(#{@rule_name})"
    end
  end

  # Match any single character
  class Dot
    property label : String?

    def label(label : String) : Dot
      @label = label
      self
    end

    def preorder_traverse(matcher, visit : Expr -> _, visited_nodes : Set(Expr))
      return if visited_nodes.includes?(self)
      visited_nodes << self
      visit.call(self)
    end

    # returns String | Nil
    def eval(matcher) : ParseTree?
      # return nil if matcher.fail_all_rules?

      matcher.push_onto_call_stack(TerminalCall.new(self, matcher.pos))

      orig_pos = matcher.pos

      GlobalDebug.puts "try #{to_s} at #{orig_pos}"

      if matcher.eof?
        matcher.pos = orig_pos
        matcher.pop_off_of_call_stack
        matcher.log_match_failure(orig_pos, self)
        return nil
      end
      consumed_str = matcher.consume(1)

      matcher.pop_off_of_call_stack
      if consumed_str
        GlobalDebug.puts "matched dot(#{consumed_str}) at #{orig_pos}"
        TerminalTree.new(consumed_str, matcher.input, orig_pos, matcher.pos - 1).label(@label)
      else
        GlobalDebug.puts "failed #{to_s} at #{orig_pos}"
        matcher.log_match_failure(orig_pos, self)
        nil
      end
    end

    def to_s
      "dot"
    end
  end

  # Match string literals
  class Terminal
    getter str : String
    property label : String?

    def initialize(str)
      @str = str
    end

    def label(label : String) : Terminal
      @label = label
      self
    end

    def preorder_traverse(matcher, visit : Expr -> _, visited_nodes : Set(Expr))
      return if visited_nodes.includes?(self)
      visited_nodes << self
      visit.call(self)
    end

    # returns String | Nil
    def eval(matcher) : ParseTree?
      # return nil if matcher.fail_all_rules?

      matcher.push_onto_call_stack(TerminalCall.new(self, matcher.pos))

      orig_pos = matcher.pos

      GlobalDebug.puts "try #{to_s} at #{orig_pos}"

      terminal_matches = @str.each_char.all? do |c|
        if matcher.eof?
          matcher.pos = orig_pos
          matcher.pop_off_of_call_stack
          GlobalDebug.puts "failed #{to_s} at #{orig_pos} ; eof"
          matcher.log_match_failure(orig_pos, self)
          return nil
        end
        matcher.consume(c)
      end

      matcher.pop_off_of_call_stack
      if terminal_matches
        GlobalDebug.puts "matched #{to_s} at #{orig_pos}"
        TerminalTree.new(@str, matcher.input, orig_pos, matcher.pos - 1).label(@label)
      else
        GlobalDebug.puts "failed #{to_s} at #{orig_pos}"
        matcher.log_match_failure(orig_pos, self)
        nil
      end
    end

    def to_s
      "term(\"#{@str}\")"
    end
  end

  # Mutually exclusive terminal alternation
  # A terminal expression, like `Terminal`, that captures a set of mutually exclusive equal-length strings.
  # If this is ever extended to strings of different length, then none of the strings in the set may be a 
  # substring of another string in the set.
  # The range operator can be implemented in terms of this expression.
  # e.g. "a" | "b" | "c"
  # e.g. "abc" | "def" | "xyz"
  class MutexAlt
    getter strings : Set(String)    # all strings in the set have the same length
    property label : String?

    def initialize(@strings : Set(String))
    end

    def label(label : String) : MutexAlt
      @label = label
      self
    end

    def preorder_traverse(matcher, visit : Expr -> _, visited_nodes : Set(Expr))
      return if visited_nodes.includes?(self)
      visited_nodes << self
      visit.call(self)
      exprs.each {|expr| expr.preorder_traverse(matcher, visit, visited_nodes) if expr.responds_to?(:preorder_traverse) }
    end

    # returns String | Nil
    def eval(matcher) : ParseTree?
      # return nil if matcher.fail_all_rules? || @strings.empty?
      return nil if @strings.empty?

      matcher.push_onto_call_stack(MutexAltCall.new(self, matcher.pos))

      string_length = @strings.first.size

      orig_pos = matcher.pos

      GlobalDebug.puts "try #{to_s} at #{orig_pos}"

      consumed_string = matcher.consume(string_length)
      parse_tree = if consumed_string && @strings.includes?(consumed_string)
        GlobalDebug.puts "matched ma(#{consumed_string}) at #{orig_pos}"
        MutexAltTree.new(consumed_string, matcher.input, orig_pos, matcher.pos - 1).label(@label)
      else
        GlobalDebug.puts "failed #{to_s} at #{orig_pos}"
        matcher.pos = orig_pos
        nil
      end

      matcher.pop_off_of_call_stack
      matcher.log_match_failure(orig_pos, self) unless parse_tree
      parse_tree
    end

    def to_s
      "ma(\"#{@strings.first(10).join("|")}|...\")"
    end
  end

  # Ordered choice
  # e.g. "foo bar baz" / "foo bar" / "foo"
  class Choice
    @exps : Array(Expr)
    property label : String?

    def initialize(exps)
      @exps = exps
    end

    def label(label : String) : Choice
      @label = label
      self
    end

    def preorder_traverse(matcher, visit : Expr -> _, visited_nodes : Set(Expr))
      return if visited_nodes.includes?(self)
      visited_nodes << self
      visit.call(self)
      @exps.each {|expr| expr.preorder_traverse(matcher, visit, visited_nodes) }
    end

    def eval(matcher) : ParseTree?
      # return nil if matcher.fail_all_rules?

      matcher.push_onto_call_stack(ChoiceCall.new(self, matcher.pos))

      orig_pos = matcher.pos
      
      GlobalDebug.puts "try #{to_s} at #{orig_pos}"

      @exps.reject {|expr| expr.is_a?(PosLookAhead) || expr.is_a?(NegLookAhead) }.each do |expr|
        # if matcher.fail_all_rules?
        #   matcher.pop_off_of_call_stack
        #   return nil
        # end
        matcher.pos = orig_pos
        parse_tree = expr.eval(matcher)
        if parse_tree
          matcher.pop_off_of_call_stack
          GlobalDebug.puts "matched choice(#{expr.to_s}) => #{parse_tree.syntax_tree} at #{orig_pos}"
          return ChoiceTree.new(parse_tree, matcher.input, orig_pos, parse_tree.finishing_pos).label(@label)
        end
      end

      matcher.pop_off_of_call_stack
      GlobalDebug.puts "failed #{to_s} at #{orig_pos}"
      nil
    end

    def to_s
      "alt(#{@exps.map(&.to_s).join(" / ")})"
    end
  end

  class Sequence
    @exps : Array(Expr)
    property label : String?

    def initialize(exps)
      @exps = exps
    end

    def label(label : String) : Sequence
      @label = label
      self
    end

    def preorder_traverse(matcher, visit : Expr -> _, visited_nodes : Set(Expr))
      return if visited_nodes.includes?(self)
      visited_nodes << self
      visit.call(self)
      @exps.each {|expr| expr.preorder_traverse(matcher, visit, visited_nodes) }
    end

    # returns Array(ParseTree) | Nil
    def eval(matcher) : ParseTree?
      # return nil if matcher.fail_all_rules?

      matcher.push_onto_call_stack(SequenceCall.new(self, matcher.pos))

      ans = [] of ParseTree
      start_pos = matcher.pos

      GlobalDebug.puts "try #{to_s} at #{start_pos}"

      @exps.each_with_index do |expr, term_index|
        matcher.skip_whitespace_if_in_syntactic_context(expr) if term_index > 0
        parse_tree = expr.eval(matcher)
        if parse_tree.nil?    # || matcher.fail_all_rules?
          matcher.pos = start_pos
          matcher.pop_off_of_call_stack
          GlobalDebug.puts "failed #{to_s} at #{start_pos}"
          return nil
        end
        # puts "#{"4" * 80} - Seq#match '#{parse_tree.text}'"
        ans.push(parse_tree) unless expr.is_a?(NegLookAhead) || expr.is_a?(PosLookAhead)
      end

      matcher.pop_off_of_call_stack
      parse_tree = SequenceTree.new(ans, matcher.input, start_pos, matcher.pos - 1).label(@label)
      GlobalDebug.puts "matched #{to_s} => #{parse_tree.syntax_tree} at #{start_pos}"
      parse_tree
    end

    def to_s
      "seq(#{@exps.map(&.to_s).join(" ")})"
    end
  end

  # Non-consuming negative lookahead match of e
  class NegLookAhead
    @exp : Expr
    property label : String?

    def initialize(exp)
      @exp = exp
    end

    def label(label : String) : NegLookAhead
      @label = label
      self
    end

    def preorder_traverse(matcher, visit : Expr -> _, visited_nodes : Set(Expr))
      return if visited_nodes.includes?(self)
      visited_nodes << self
      visit.call(self)
      @exp.preorder_traverse(matcher, visit, visited_nodes)
    end

    # this should return true if the expr does not match, and nil otherwise; do not return false, because nil indicates parse failure
    def eval(matcher) : ParseTree?
      # return nil if matcher.fail_all_rules?

      matcher.push_onto_call_stack(NegLookAheadCall.new(self, matcher.pos))

      orig_pos = matcher.pos
      expr_does_not_match = !@exp.eval(matcher)
      matcher.pos = orig_pos
      matcher.pop_off_of_call_stack
      # return nil if matcher.fail_all_rules?
      NegLookAheadTree.new(expr_does_not_match, matcher.input, orig_pos).label(@label) if expr_does_not_match
    end

    def to_s
      "!#{@exp.to_s}"
    end
  end

  # Non-consuming positive lookahead match of e
  class PosLookAhead
    @exp : Expr
    property label : String?

    def initialize(exp)
      @exp = exp
    end

    def label(label : String) : PosLookAhead
      @label = label
      self
    end

    def preorder_traverse(matcher, visit : Expr -> _, visited_nodes : Set(Expr))
      return if visited_nodes.includes?(self)
      visited_nodes << self
      visit.call(self)
      @exp.preorder_traverse(matcher, visit, visited_nodes)
    end

    # this should return true if the expr matches, and nil otherwise; do not return false, because nil indicates parse failure
    def eval(matcher) : ParseTree?
      # return nil if matcher.fail_all_rules?

      matcher.push_onto_call_stack(PosLookAheadCall.new(self, matcher.pos))

      orig_pos = matcher.pos
      expr_matches = !!@exp.eval(matcher)
      matcher.pos = orig_pos
      matcher.pop_off_of_call_stack
      # return nil if matcher.fail_all_rules?
      PosLookAheadTree.new(expr_matches, matcher.input, orig_pos).label(@label) if expr_matches
    end

    def to_s
      "&#{@exp.to_s}"
    end
  end

  class Optional
    @exp : Expr
    property label : String?

    def initialize(exp)
      @exp = exp
    end

    def label(label : String) : Optional
      @label = label
      self
    end

    def preorder_traverse(matcher, visit : Expr -> _, visited_nodes : Set(Expr))
      return if visited_nodes.includes?(self)
      visited_nodes << self
      visit.call(self)
      @exp.preorder_traverse(matcher, visit, visited_nodes)
    end

    # returns Array(ParseTree) | Nil
    def eval(matcher) : ParseTree?
      # return nil if matcher.fail_all_rules?

      matcher.push_onto_call_stack(OptionalCall.new(self, matcher.pos))

      parse_tree = nil

      orig_pos = matcher.pos
      tmp_parse_tree = @exp.eval(matcher)
      if tmp_parse_tree
        parse_tree = tmp_parse_tree unless tmp_parse_tree.is_a?(NegLookAheadTree) || tmp_parse_tree.is_a?(PosLookAheadTree)
      else
        matcher.pos = orig_pos
      end

      matcher.pop_off_of_call_stack

      # return nil if matcher.fail_all_rules?

      OptionalTree.new(parse_tree, matcher.input, orig_pos, matcher.pos - 1).label(@label)
    end

    def to_s
      "#{@exp.to_s}?"
    end
  end

  # this represents the kleene star operator - 0+ repetitions
  class Repetition
    @exp : Expr
    property label : String?

    def initialize(exp)
      @exp = exp
    end

    def label(label : String) : Repetition
      @label = label
      self
    end

    def preorder_traverse(matcher, visit : Expr -> _, visited_nodes : Set(Expr))
      return if visited_nodes.includes?(self)
      visited_nodes << self
      visit.call(self)
      @exp.preorder_traverse(matcher, visit, visited_nodes)
    end

    # returns Array(ParseTree) | Nil
    def eval(matcher) : ParseTree?
      # return nil if matcher.fail_all_rules?

      matcher.push_onto_call_stack(RepetitionCall.new(self, matcher.pos))
      
      start_pos = matcher.pos

      ans = [] of ParseTree
      term_count = 0
      loop do
        orig_pos = matcher.pos
        matcher.skip_whitespace_if_in_syntactic_context(@exp) if term_count > 0
        parse_tree = @exp.eval(matcher)
        if parse_tree
          # if matcher.fail_all_rules?
          #   matcher.pop_off_of_call_stack
          #   return nil
          # end
          ans.push(parse_tree) unless @exp.is_a?(NegLookAhead) || @exp.is_a?(PosLookAhead)
        else
          matcher.pos = orig_pos
          # if matcher.fail_all_rules?
          #   matcher.pop_off_of_call_stack
          #   return nil
          # end
          break
        end
        term_count += 1
      end

      matcher.pop_off_of_call_stack
      RepetitionTree.new(ans, matcher.input, start_pos, matcher.pos - 1).label(@label)
    end

    def to_s
      "#{@exp.to_s}*"
    end
  end

  # this represents 1+ repetitions
  class RepetitionOnePlus
    @exp : Expr
    property label : String?

    def initialize(exp)
      @exp = exp
    end

    def label(label : String) : RepetitionOnePlus
      @label = label
      self
    end

    def preorder_traverse(matcher, visit : Expr -> _, visited_nodes : Set(Expr))
      return if visited_nodes.includes?(self)
      visited_nodes << self
      visit.call(self)
      @exp.preorder_traverse(matcher, visit, visited_nodes)
    end

    # returns Array(ParseTree) | Nil
    def eval(matcher) : ParseTree?
      # return nil if matcher.fail_all_rules?

      matcher.push_onto_call_stack(RepetitionCall.new(self, matcher.pos))

      ans = [] of ParseTree
      start_pos = matcher.pos
      term_count = 0
      loop do
        orig_pos = matcher.pos
        matcher.skip_whitespace_if_in_syntactic_context(@exp) if term_count > 0
        parse_tree = @exp.eval(matcher)
        if parse_tree
          # if matcher.fail_all_rules?
          #   matcher.pop_off_of_call_stack
          #   return nil
          # end
          ans.push(parse_tree) unless @exp.is_a?(NegLookAhead) || @exp.is_a?(PosLookAhead)
        else
          matcher.pos = orig_pos
          # if matcher.fail_all_rules?
          #   matcher.pop_off_of_call_stack
          #   return nil
          # end
          break
        end
        term_count += 1
      end

      matcher.pop_off_of_call_stack
      if ans.size >= 1
        RepetitionTree.new(ans, matcher.input, start_pos, matcher.pos - 1).label(@label)
      else
        matcher.pos = start_pos
        nil
      end
    end

    def to_s
      "#{@exp.to_s}+"
    end
  end

end
