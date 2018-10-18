require "./spec_helper"

include Arborist
include Arborist::DSL

describe Arborist do
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
      c1 = choice(t1, t2)   # "abc" | "def"

      m = Matcher.new.add_rule("start", c1)

      m.match("abc").try(&.syntax_tree).should eq "abc"
      m.match("def").try(&.syntax_tree).should eq "def"
    end

    it "prioritizes first option over second option in the case that both match" do
      r1 = seq(term("abc"), term("def"))  # "abc" "def"
      r2 = term("abcdef")

      c1 = choice(r1, r2)   # ("abc" "def") | "abcdef"
      m1 = Matcher.new.add_rule("start", c1)

      c2 = choice(r2, r1)   # "abcdef" | ("abc" "def")
      m2 = Matcher.new.add_rule("start", c2)

      m1.match("abcdef").try(&.syntax_tree).should eq ["abc", "def"] of SyntaxTree
      m2.match("abcdef").try(&.syntax_tree).should eq "abcdef"
    end
  end

  describe "optional" do
    it "allows a rule to be optionally matched" do
      r1 = seq(opt(term("abc")), term("def"))   # "abc"? "def"
      m1 = Matcher.new.add_rule("start", r1)

      m1.match("abcdef").try(&.syntax_tree).should eq [["abc"] of SyntaxTree, "def"] of SyntaxTree   # should == [["abc"], "def"]
      m1.match("def").try(&.syntax_tree).should eq [[] of SyntaxTree, "def"] of SyntaxTree   # should == [[], "def"]
    end
  end

  describe "dot" do
    it "matches any character" do
      r1 = seq(dot, dot, dot)   # /.../
      m1 = Matcher.new.add_rule("start", r1)

      m1.match("abc").try(&.syntax_tree).should eq ["a", "b", "c"]
      m1.match("xyz").try(&.syntax_tree).should eq ["x", "y", "z"]
    end
  end

  describe "negative lookahead" do
    it "allows a subsequent rule to be matched so long as it doesn't match the predicate captured in the negative lookahead rule" do
      r1 = seq(neg(term("abc")), seq(dot, dot, dot))   # ~"abc" /.../
      m1 = Matcher.new.add_rule("start", r1)

      m1.match("abc").try(&.syntax_tree).should be_nil
      m1.match("xyz").try(&.syntax_tree).should eq [["x", "y", "z"]]
    end

    it "can be stacked to ensure multiple things do not match" do
      r1 = seq(neg(term("abc")), neg(term("xyz")), seq(dot, dot, dot))   # ~"abc" ~"xyz" /.../
      m1 = Matcher.new.add_rule("start", r1)

      m1.match("abc").try(&.syntax_tree).should be_nil
      m1.match("xyz").try(&.syntax_tree).should be_nil
      m1.match("foo").try(&.syntax_tree).should eq [["f", "o", "o"]]
    end
  end

  describe "positive lookahead" do
    it "allows a subsequent rule to be matched so long as it also matches the predicate captured in the positive lookahead rule" do
      r1 = seq(pos(term("abc")), seq(dot, dot, dot))   # &"abc" /.../
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
      expr = seq(apply("expr"), term("-"), apply("num"))    # expr -> expr - num
      num = plus(range('0'..'9'))                                     # num -> [0-9]+
      m1 = Matcher.new.add_rule("expr", expr).add_rule("num", num)

      m1.match("1-2-3", "expr").should be_nil
    end

    it "allows rules that are left-recursion and not right-recursive" do
      expr = choice(seq(apply("expr"), term("-"), apply("num")), apply("num"))    # expr -> expr - num | num
      num = plus(range('0'..'9'))                                                                       # num -> [0-9]+
      m1 = Matcher.new.add_rule("expr", expr).add_rule("num", num)

      m1.match("1-2-3", "expr").try(&.syntax_tree).should eq [[["1"], "-", ["2"]], "-", ["3"]]   # should parse as (((1)-2)-3)
    end

    it "matches e -> e '2' | '1'" do
      e = choice(seq(apply("e"), term("2")), term("1"))    # e -> e "2" | "1"
      m1 = Matcher.new.add_rule("e", e)

      m1.match("1", "e").try(&.syntax_tree).should eq "1"
      m1.match("12", "e").try(&.syntax_tree).should eq ["1", "2"]
      m1.match("122", "e").try(&.syntax_tree).should eq [["1", "2"], "2"]
    end

    it "allows rules that are right-recursive and not left-recursive" do
      expr = choice(seq(apply("num"), term("-"), apply("expr")), apply("num"))    # expr -> expr - num | num
      num = plus(range('0'..'9'))                                                                       # num -> [0-9]+
      m1 = Matcher.new.add_rule("expr", expr).add_rule("num", num)

      m1.match("1-2-3", "expr").try(&.syntax_tree).should eq [["1"], "-", [["2"], "-", ["3"]]]   # should parse as (1-(2-(3))
    end

    it "allows rules that are left-recursive and simultaneously recursive in a second point, but not right-recursive" do
      e = choice(seq(apply("e"), term("-"), apply("e"), term("m")), term("5"))    # e -> e - e | 5
      m1 = Matcher.new.add_rule("e", e)

      m1.match("5-5m-5m", "e").try(&.syntax_tree).should eq [["5", "-", "5", "m"], "-", "5", "m"]   # should parse as (((5)-5m)-5m)
    end

    it "allows rules that are left and right recursive" do
      e = choice(seq(apply("e"), term("-"), apply("e")), term("5"))    # e -> e - e | 5
      m1 = Matcher.new.add_rule("e", e)

      m1.match("5-5-5", "e").try(&.syntax_tree).should eq [["5", "-", "5"], "-", "5"]   # should parse as (((5)-5)-5)
    end

    it "correctly parses e -> e - e | e + e | num" do
      # e -> e - e | e + e | num
      # num -> [0-9]+
      e = choice(seq(apply("e"), term("-"), apply("e")), 
                 seq(apply("e"), term("+"), apply("e")), 
                 apply("num") )
      num = plus(range('0'..'9'))
      m1 = Matcher.new.add_rule("e", e).add_rule("num", num)

      # 1-2+3-4+5 should parse as (((((1)-2)+3)-4)+5)
      m1.match("1-2+3-4+5", "e").try(&.syntax_tree).should eq [[[[["1"], "-", ["2"]], "+", ["3"]], "-", ["4"]], "+", ["5"]]
    end

    it "correctly parses e -> e - e (- e)? | num" do
      # e -> e - e (- e)? | num
      # num -> [0-9]+
      e = choice(seq(apply("e"), term("-"), apply("e"), opt(seq(term("-"), apply("e")))), 
                 apply("num"))
      num = plus(range('0'..'9'))
      m1 = Matcher.new.add_rule("e", e).add_rule("num", num)

      # 1-2-3 should parse as (1-2(-3))
      m1.match("1-2-3", "e").try(&.syntax_tree).should eq [["1"], "-", ["2"], [["-", ["3"]]]]
    end
  end

end
