require "./spec_helper"

include PegParser
include PegParser::DSL

describe PegParser do
  describe "terminal" do
    it "parses a string" do
      t1 = term("abc")

      m = Matcher.new.add_rule("start", t1)

      m.match("abc").should eq "abc"
    end
  end

  describe "choice" do
    it "parses one or the other string" do
      t1 = term("abc")
      t2 = term("def")
      c1 = choice([t1, t2] of Rule)   # "abc" | "def"

      m = Matcher.new.add_rule("start", c1)

      m.match("abc").should eq "abc"
      m.match("def").should eq "def"
    end

    it "prioritizes first option over second option in the case that both match" do
      r1 = seq([term("abc"), term("def")] of Rule)  # "abc" "def"
      r2 = term("abcdef")

      c1 = choice([r1, r2] of Rule)   # ("abc" "def") | "abcdef"
      m1 = Matcher.new.add_rule("start", c1)

      c2 = choice([r2, r1] of Rule)   # "abcdef" | ("abc" "def")
      m2 = Matcher.new.add_rule("start", c2)

      m1.match("abcdef").should eq ["abc", "def"] of ParseTree
      m2.match("abcdef").should eq "abcdef"
    end
  end

  describe "optional" do
    it "allows a rule to be optionally matched" do
      r1 = seq([opt(term("abc")), term("def")] of Rule)   # "abc"? "def"
      m1 = Matcher.new.add_rule("start", r1)

      m1.match("abcdef").should eq [["abc"] of ParseTree, "def"] of ParseTree   # should == [["abc"], "def"]
      m1.match("def").should eq [[] of ParseTree, "def"] of ParseTree   # should == [[], "def"]
    end
  end

  describe "dot" do
    it "matches any character" do
      r1 = seq([dot, dot, dot] of Rule)   # /.../
      m1 = Matcher.new.add_rule("start", r1)

      m1.match("abc").should eq ["a", "b", "c"]
      m1.match("xyz").should eq ["x", "y", "z"]
    end
  end

  describe "negative lookahead" do
    it "allows a subsequent rule to be matched so long as it doesn't match the predicate captured in the negative lookahead rule" do
      r1 = seq([neg(term("abc")), seq([dot, dot, dot] of Rule)] of Rule)   # &"abc" /.../
      m1 = Matcher.new.add_rule("start", r1)

      m1.match("abc").should be_nil
      m1.match("xyz").should eq [["x", "y", "z"]]
    end
  end

  describe "positive lookahead" do
    it "allows a subsequent rule to be matched so long as it also matches the predicate captured in the positive lookahead rule" do
      r1 = seq([pos(term("abc")), seq([dot, dot, dot] of Rule)] of Rule)   # &"abc" /.../
      m1 = Matcher.new.add_rule("start", r1)

      m1.match("abc").should eq [["a", "b", "c"]]
      m1.match("xyz").should be_nil
    end
  end
end
