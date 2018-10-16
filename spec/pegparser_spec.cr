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

  describe "range" do
    it "matches any character in the range" do
      num = range('0'..'9')
      m1 = Matcher.new.add_rule("num", num)
      m1.match("1", "num").try(&.syntax_tree).should eq "1"
    end

    it "doesn't match any character outside of the range" do
      num = range('0'..'9')
      m1 = Matcher.new.add_rule("num", num)
      m1.match("a", "num").try(&.syntax_tree).should be_nil
    end
  end

  describe "star" do
    it "matches 0+ instances of the wrapped" do
      r1 = star(term("a"))
      m1 = Matcher.new.add_rule("rule1", r1)
      m1.match("", "rule1").try(&.syntax_tree).should eq [] of SyntaxTree
      m1.match("a", "rule1").try(&.syntax_tree).should eq ["a"] of SyntaxTree
      m1.match("aa", "rule1").try(&.syntax_tree).should eq ["a", "a"] of SyntaxTree
      m1.match("b", "rule1").try(&.syntax_tree).should be_nil
    end

    it "works in conjunction with the range expression" do
      r1 = star(range('0'..'9'))
      m1 = Matcher.new.add_rule("rule1", r1)
      m1.match("", "rule1").try(&.syntax_tree).should eq [] of SyntaxTree
      m1.match("1", "rule1").try(&.syntax_tree).should eq ["1"] of SyntaxTree
      m1.match("123", "rule1").try(&.syntax_tree).should eq ["1", "2", "3"] of SyntaxTree
    end
  end

  describe "plus" do
    it "matches any character in the range" do
      r1 = plus(term("a"))
      m1 = Matcher.new.add_rule("rule1", r1)
      m1.match("", "rule1").try(&.syntax_tree).should be_nil
      m1.match("a", "rule1").try(&.syntax_tree).should eq ["a"]
      m1.match("aa", "rule1").try(&.syntax_tree).should eq ["a", "a"]
      m1.match("b", "rule1").try(&.syntax_tree).should be_nil
    end

    it "works in conjunction with the range expression" do
      r1 = plus(range('0'..'9'))
      m1 = Matcher.new.add_rule("rule1", r1)
      m1.match("", "rule1").try(&.syntax_tree).should be_nil
      m1.match("1", "rule1").try(&.syntax_tree).should eq ["1"] of SyntaxTree
      m1.match("123", "rule1").try(&.syntax_tree).should eq ["1", "2", "3"] of SyntaxTree
    end
  end

  describe "left-recursion support" do
    it "rejects left recursive rules that would never backtrack to an alternate branch if the left recursive application fails" do
      expr = seq([apply("expr"), term("-"), apply("num")] of Expr)    # expr -> expr - num
      num = plus(range('0'..'9'))                                     # num -> [0-9]+
      m1 = Matcher.new.add_rule("expr", expr).add_rule("num", num)

      m1.match("1-2-3", "expr").should be_nil
    end

    it "allows rules that are left-recursion and not right-recursive" do
      expr = choice([ seq([ apply("expr"), term("-"), apply("num")] of Expr), apply("num")] of Expr)    # expr -> expr - num | num
      num = plus(range('0'..'9'))                                                                       # num -> [0-9]+
      m1 = Matcher.new.add_rule("expr", expr).add_rule("num", num)

      m1.match("1-2-3", "expr").try(&.syntax_tree).should eq [[["1"], "-", ["2"]], "-", ["3"]]   # should parse as (((1)-2)-3)
    end

    it "matches e -> e '2' | '1'" do
      e = choice([ seq([ apply("e"), term("2")] of Expr), term("1")] of Expr)    # e -> e "2" | "1"
      m1 = Matcher.new.add_rule("e", e)

      m1.match("1", "e").try(&.syntax_tree).should eq "1"
      m1.match("12", "e").try(&.syntax_tree).should eq ["1", "2"]
      m1.match("122", "e").try(&.syntax_tree).should eq [["1", "2"], "2"]
    end

    it "allows rules that are right-recursive and not left-recursive" do
      expr = choice([ seq([ apply("num"), term("-"), apply("expr")] of Expr), apply("num")] of Expr)    # expr -> expr - num | num
      num = plus(range('0'..'9'))                                                                       # num -> [0-9]+
      m1 = Matcher.new.add_rule("expr", expr).add_rule("num", num)

      m1.match("1-2-3", "expr").try(&.syntax_tree).should eq [["1"], "-", [["2"], "-", ["3"]]]   # should parse as (1-(2-(3))
    end

    it "allows rules that are left-recursive and simultaneously recursive in a second point, but not right-recursive" do
      e = choice([ seq([ apply("e"), term("-"), apply("e"), term("m")] of Expr), term("5")] of Expr)    # e -> e - e | 5
      m1 = Matcher.new.add_rule("e", e)

      m1.match("5-5m-5m", "e").try(&.syntax_tree).should eq [["5", "-", "5", "m"], "-", "5", "m"]   # should parse as (((5)-5m)-5m)
    end

    it "allows rules that are left and right recursive" do
      e = choice([ seq([ apply("e"), term("-"), apply("e")] of Expr), term("5")] of Expr)    # e -> e - e | 5
      m1 = Matcher.new.add_rule("e", e)

      m1.match("5-5-5", "e").try(&.syntax_tree).should eq [[["5"], "-", ["5"]], "-", ["5"]]   # should parse as (((5)-5)-5)
    end
  end

  describe "rule" do
    describe "direct_definite_right_recursive?" do
      it "returns true when a sequence rule's right most term is definitely right recursive; false otherwise" do
        r1 = seq([term("foo"), term("bar")] of Expr)
        r2 = seq([term("foo"), term("bar"), apply("rule2")] of Expr)
        r3 = seq([term("foo"), apply("rule3"), term("bar")] of Expr)
        m1 = Matcher.new.add_rule("rule1", r1).add_rule("rule2", r2).add_rule("rule3", r3)

        m1.get_rule("rule1").direct_definite_right_recursive?.should be_false
        m1.get_rule("rule2").direct_definite_right_recursive?.should be_true
        m1.get_rule("rule3").direct_definite_right_recursive?.should be_false
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

      it "return false if a rule is merely potentially right recursive" do
        r1 = seq([term("foo"), apply("rule1"), opt(term("bar"))] of Expr)
        m1 = Matcher.new.add_rule("rule1", r1)

        m1.get_rule("rule1").direct_definite_right_recursive?.should be_false
      end

      it "a rule is directly and definitely right recursive if the rule is directly recursive, right-recursive, and the right-recursive term is static/fixed/unchanging and unconditional. In other words, the right-recursive term must not be dependent on the input being matched." do
        # test case 1
        r1 = seq([term("foo"), apply("rule1"), opt(apply("rule1"))] of Expr)
        m1 = Matcher.new.add_rule("rule1", r1)

        m1.get_rule("rule1").direct_definite_right_recursive?.should be_false


        # test case 2
        expr = choice([ seq([ apply("expr"), term("-"), apply("expr")] of Expr), apply("num")] of Expr)   # expr -> expr "-" num / num
        num = plus(range('0'..'9'))                                                                       # num -> [0-9]+
        m1 = Matcher.new.add_rule("expr", expr).add_rule("num", num)

        m1.get_rule("expr").direct_definite_right_recursive?.should be_true
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
