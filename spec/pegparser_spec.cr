require "./spec_helper"

include PegParser
include PegParser::DSL

describe PegParser do
  describe "terminal" do
    it "parses a string" do
      t1 = term("abc")

      m = Matcher.new.add_rule("start", t1)

      m.match("abc").try(&.syntax_tree).should eq "abc"
    end
  end

  describe "choice" do
    it "parses one or the other string" do
      t1 = term("abc")
      t2 = term("def")
      c1 = choice([t1, t2] of Expr)   # "abc" | "def"

      m = Matcher.new.add_rule("start", c1)

      m.match("abc").try(&.syntax_tree).should eq "abc"
      m.match("def").try(&.syntax_tree).should eq "def"
    end

    it "prioritizes first option over second option in the case that both match" do
      r1 = seq([term("abc"), term("def")] of Expr)  # "abc" "def"
      r2 = term("abcdef")

      c1 = choice([r1, r2] of Expr)   # ("abc" "def") | "abcdef"
      m1 = Matcher.new.add_rule("start", c1)

      c2 = choice([r2, r1] of Expr)   # "abcdef" | ("abc" "def")
      m2 = Matcher.new.add_rule("start", c2)

      m1.match("abcdef").try(&.syntax_tree).should eq ["abc", "def"] of SyntaxTree
      m2.match("abcdef").try(&.syntax_tree).should eq "abcdef"
    end
  end

  describe "optional" do
    it "allows a rule to be optionally matched" do
      r1 = seq([opt(term("abc")), term("def")] of Expr)   # "abc"? "def"
      m1 = Matcher.new.add_rule("start", r1)

      m1.match("abcdef").try(&.syntax_tree).should eq [["abc"] of SyntaxTree, "def"] of SyntaxTree   # should == [["abc"], "def"]
      m1.match("def").try(&.syntax_tree).should eq [[] of SyntaxTree, "def"] of SyntaxTree   # should == [[], "def"]
    end
  end

  describe "dot" do
    it "matches any character" do
      r1 = seq([dot, dot, dot] of Expr)   # /.../
      m1 = Matcher.new.add_rule("start", r1)

      m1.match("abc").try(&.syntax_tree).should eq ["a", "b", "c"]
      m1.match("xyz").try(&.syntax_tree).should eq ["x", "y", "z"]
    end
  end

  describe "negative lookahead" do
    it "allows a subsequent rule to be matched so long as it doesn't match the predicate captured in the negative lookahead rule" do
      r1 = seq([neg(term("abc")), seq([dot, dot, dot] of Expr)] of Expr)   # &"abc" /.../
      m1 = Matcher.new.add_rule("start", r1)

      m1.match("abc").try(&.syntax_tree).should be_nil
      m1.match("xyz").try(&.syntax_tree).should eq [["x", "y", "z"]]
    end
  end

  describe "positive lookahead" do
    it "allows a subsequent rule to be matched so long as it also matches the predicate captured in the positive lookahead rule" do
      r1 = seq([pos(term("abc")), seq([dot, dot, dot] of Expr)] of Expr)   # &"abc" /.../
      m1 = Matcher.new.add_rule("start", r1)

      m1.match("abc").try(&.syntax_tree).should eq [["a", "b", "c"]]
      m1.match("xyz").try(&.syntax_tree).should be_nil
    end
  end

  describe "left-recursion support" do
    it "allows rules that are left-recursion and not right-recursive" do
      expr = seq([apply("expr"), term("-"), apply("num")] of Expr)    # expr -> expr - num
      num = plus(range('0'..'9'))                                     # num -> [0-9]+
      m1 = Matcher.new.add_rule("expr", expr).add_rule("num", num)

      m1.match("1-2-3", "expr").try(&.syntax_tree).should eq [[["1"], "-", "2"], "-", "3"]   # should parse as (((1)-2)-3)
    end
  end

  describe "rule" do
    describe "direct_definite_right_recursive?" do
      it "returns true when a sequence rule's right most term is definitely right recursive; false otherwise" do
        r1 = seq([term("foo"), term("bar")] of Expr)
        r2 = seq([term("foo"), term("bar"), apply("rule2")] of Expr)
        m1 = Matcher.new.add_rule("rule1", r1).add_rule("rule2", r2)

        m1.get_rule("rule1").direct_definite_right_recursive?.should be_false
        m1.get_rule("rule2").direct_definite_right_recursive?.should be_true
      end

      it "returns false if a rule is indirectly right recursive (through mutual recursion with another rule)" do
        r1 = seq([term("foo"), term("bar"), apply("rule2")] of Expr)
        r2 = seq([term("baz"), term("qux"), apply("rule1")] of Expr)
        m1 = Matcher.new.add_rule("rule1", r1).add_rule("rule2", r2)

        m1.get_rule("rule1").direct_definite_right_recursive?.should be_false
        m1.get_rule("rule2").direct_definite_right_recursive?.should be_false
      end

      it "returns false if a rule is optionally right recursive" do
        r1 = seq([term("foo"), term("bar"), opt(apply("rule1"))] of Expr)
        m1 = Matcher.new.add_rule("rule1", r1)

        m1.get_rule("rule1").direct_definite_right_recursive?.should be_false
      end

      it "returns false if a rule has a right-most 0+ repetition expression that wraps a recursive rule application" do
        r1 = seq([term("foo"), term("bar"), star(apply("rule1"))] of Expr)
        m1 = Matcher.new.add_rule("rule1", r1)

        m1.get_rule("rule1").direct_definite_right_recursive?.should be_false
      end

      it "returns true if a rule has a right-most 1+ repetition expression that wraps a recursive rule application" do
        r1 = seq([term("foo"), term("bar"), plus(apply("rule1"))] of Expr)
        m1 = Matcher.new.add_rule("rule1", r1)

        m1.get_rule("rule1").direct_definite_right_recursive?.should be_true
      end
    end
  end
end
