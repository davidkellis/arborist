# Based on https://github.com/ohmlang/sle17/blob/master/src/standard.js
# https://ohmlang.github.io/pubs/sle2017/incremental-packrat-parsing.pdf
# https://tratt.net/laurie/research/pubs/html/tratt__direct_left_recursive_parsing_expression_grammars/
# https://github.com/mjackson/citrus/blob/master/lib/citrus.rb

module PegParser
  VERSION = "0.1.0"

  module DSL
    ALPHABET = ((' '..'~').map(&.to_s) + ["\n"] + ["\t"])

    def term(string : String) : Rule
      Terminal.new(string)
    end

    def choice(alternatives : Array(Rule)) : Rule
      Choice.new(alternatives)
    end

    def apply(rule_name : String) : Rule
      RuleApplication.new(rule_name)
    end

    def range(chars : Range(Char, Char)) : Rule
      terms : Array(Rule) = chars.map {|char| term(char.to_s) }
      choice(terms)
    end

    def dot(alphabet = ALPHABET) : Rule
      terms : Array(Rule) = alphabet.map {|char| term(char.to_s) }
      choice(terms)
    end

    def seq(rules : Array(Rule)) : Rule
      Sequence.new(rules)
    end
    
    # this represents the optional operator `?` - 0 or 1 repetitions
    def opt(rule : Rule) : Rule
      Optional.new(rule)
    end

    # this represents the kleene star operator - 0+ repetitions
    def star(rule : Rule) : Rule
      Repetition.new(rule)
    end

    # this represents 1+ repetitions
    def plus(rule : Rule) : Rule
      seq([rule, any(rule)] of Rule)
    end

    # not predicate - negative lookahead
    def neg(rule : Rule) : Rule
      NegLookAhead.new(rule)
    end
    
    # and predicate - positive lookahead
    def pos(rule : Rule) : Rule
      PosLookAhead.new(rule)
    end
  end

  class MemoResult
    property parse_tree : ParseTree    # the parse tree matched at the index position within the memotable array at which this memoresult exists
    property nextPos : Int32

    def initialize(@parse_tree = nil, @nextPos = 0)
    end
  end

  alias Column = Hash(String, MemoResult)

  class Matcher
    @memoTable : Hash(Int32, Column)
    @input : String
    property pos : Int32
    getter rules : Hash(String, Rule)

    def initialize(rules = {} of String => Rule)
      @rules = rules

      @input = ""
      @memoTable = {} of Int32 => Column
      @pos = 0
    end

    def add_rule(rule_name, rule : Rule)
      @rules[rule_name] = rule
      self
    end

    def match(input, start_rule_name = "start")
      @input = input
      @pos = 0
      @memoTable = {} of Int32 => Column
      parse_tree = RuleApplication.new(start_rule_name).eval(self)
      parse_tree if @pos == @input.size
    end

    def has_memoized_result(ruleName)
      col = @memoTable[@pos]?
      col && col.has_key?(ruleName)
    end

    def memoize_result(pos, ruleName, parse_tree)
      col = (@memoTable[pos] ||= {} of String => MemoResult)
      # col = @memoTable[pos]?
      # col ||= (@memoTable[pos] = {} of String => MemoResult)
      memoized_result = if parse_tree
        # {cst: cst, nextPos: @pos}
        MemoResult.new(parse_tree, @pos)
      else
        # {cst: nil}
        MemoResult.new(nil)
      end
      col[ruleName] = memoized_result
    end

    def use_memoized_result(ruleName)
      col = @memoTable[@pos]
      result = col[ruleName]
      if result.parse_tree
        @pos = result.nextPos
        result.parse_tree
      end
    end

    def consume(c)
      if @input[@pos] == c
        @pos += 1
        true
      else
        false
      end
    end
  end

  alias ParseTree = String | Array(ParseTree) | Bool | Nil    # Nil parse tree means parse error

  alias Rule = Terminal | Choice | Sequence | PosLookAhead | NegLookAhead | Optional | Repetition | RuleApplication

  # RuleApplication represents the application of a named rule
  class RuleApplication
    @ruleName : String

    def initialize(ruleName)
      @ruleName = ruleName
    end

    def eval(matcher)
      name = @ruleName
      if matcher.has_memoized_result(name)
        matcher.use_memoized_result(name)
      else
        origPos = matcher.pos
        parse_tree = matcher.rules[name].eval(matcher)
        matcher.memoize_result(origPos, name, parse_tree)
        parse_tree
      end
    end
  end

  # Match string literals
  class Terminal
    @str : String

    def initialize(str)
      @str = str
    end

    # returns String | Nil
    def eval(matcher) : ParseTree
      @str if @str.each_char.all? {|c| matcher.consume(c) }
    end
  end

  # Ordered choice
  class Choice
    @exps : Array(Rule)

    def initialize(exps)
      @exps = exps
    end

    def eval(matcher) : ParseTree
      origPos = matcher.pos
      @exps.reject {|rule| rule.is_a?(PosLookAhead) || rule.is_a?(NegLookAhead) }.each do |expr|
        matcher.pos = origPos
        parse_tree = expr.eval(matcher)
        return parse_tree if parse_tree
      end
      nil
    end
  end

  class Sequence
    @exps : Array(Rule)

    def initialize(exps)
      @exps = exps
    end

    # returns Array(ParseTree) | Nil
    def eval(matcher) : ParseTree
      ans = [] of ParseTree
      @exps.each do |expr|
        parse_tree = expr.eval(matcher)
        return nil if parse_tree.nil?
        ans.push(parse_tree) unless expr.is_a?(NegLookAhead) || expr.is_a?(PosLookAhead)
      end
      ans
    end
  end

  # Non-consuming negative lookahead match of e
  class NegLookAhead
    @exp : Rule

    def initialize(exp)
      @exp = exp
    end

    # this should return true if the rule matches, and nil otherwise; do not return false, because nil indicates parse failure
    def eval(matcher) : ParseTree
      # original from https://github.com/ohmlang/sle17/blob/master/src/standard.js
      # origPos = matcher.pos
      # if @exp.eval(matcher).nil?
      #   matcher.pos = origPos
      #   true
      # end

      # this seems like it should be implemented as:
      origPos = matcher.pos
      result = !@exp.eval(matcher)
      matcher.pos = origPos
      result || nil
    end
  end

  # Non-consuming positive lookahead match of e
  class PosLookAhead
    @exp : Rule

    def initialize(exp)
      @exp = exp
    end

    # this should return true if the rule matches, and nil otherwise; do not return false, because nil indicates parse failure
    def eval(matcher) : ParseTree
      origPos = matcher.pos
      result = !!@exp.eval(matcher)
      matcher.pos = origPos
      result || nil
    end
  end

  class Optional
    @exp : Rule

    def initialize(exp)
      @exp = exp
    end

    # returns Array(ParseTree) | Nil
    def eval(matcher) : ParseTree
      ans = [] of ParseTree

      origPos = matcher.pos
      parse_tree = @exp.eval(matcher)
      if parse_tree
        ans.push(parse_tree) unless @exp.is_a?(NegLookAhead) || @exp.is_a?(PosLookAhead)
      else
        matcher.pos = origPos
      end

      ans
    end
  end

  # this represents the kleene star operator - 0+ repetitions
  class Repetition
    @exp : Rule

    def initialize(exp)
      @exp = exp
    end

    # returns Array(ParseTree) | Nil
    def eval(matcher) : ParseTree
      ans = [] of ParseTree
      loop do
        origPos = matcher.pos
        parse_tree = @exp.eval(matcher)
        if parse_tree
          ans.push(parse_tree) unless @exp.is_a?(NegLookAhead) || @exp.is_a?(PosLookAhead)
        else
          matcher.pos = origPos
          break
        end
      end
      ans
    end
  end
end
