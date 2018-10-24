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

      m1.match("abcdef").try(&.syntax_tree).should eq ["abc", "def"] of SyntaxTree   # should == [["abc"], "def"]
      m1.match("def").try(&.syntax_tree).should eq ["def"] of SyntaxTree   # should == [[], "def"]
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
      m1.match("1-2-3", "e").try(&.syntax_tree).should eq [["1"], "-", ["2"], ["-", ["3"]]]
    end
  end

  describe "parse tree" do
    describe "parent/children relationship" do
      it "can be navigated via parent field" do
        input = "123456"
        t1 = TerminalTree.new(input[0..2], input, 0, 2)
        t2 = TerminalTree.new(input[3..5], input, 3, 5)
        s1 = SequenceTree.new([t1, t2] of ParseTree, input, 0, 5)

        s1.recursively_populate_parents

        t1.parent.should eq(s1)
        t2.parent.should eq(s1)
        s1.parent.should be_nil
      end

      it "lists descendants in an order derived from a pre-order traversal of the nodes" do
        # equality_comparison = lhs=range "==" rhs=range
        # range = nums:[0-9]+ ".." nums:[0-9]+
        input = "111..222==333..444"
        t1 = TerminalTree.new(input[0..2], input, 0, 2).label("nums")
        dots1  = TerminalTree.new(input[3..4], input, 3, 4)
        t2 = TerminalTree.new(input[5..7], input, 5, 7).label("nums")
        equals1  = TerminalTree.new(input[8..9], input, 8, 9)
        t3 = TerminalTree.new(input[10..12], input, 10, 12).label("nums")
        dots2  = TerminalTree.new(input[13..14], input, 13, 14)
        t4 = TerminalTree.new(input[15..17], input, 15, 17).label("nums")
        seq_range1 = SequenceTree.new([t1, dots1, t2] of ParseTree, input, 0, 7)
        seq_range2 = SequenceTree.new([t3, dots2, t4] of ParseTree, input, 10, 17)
        lhs = ApplyTree.new(seq_range1, "range", input, 0, 7).label("lhs")
        rhs = ApplyTree.new(seq_range2, "range", input, 10, 17).label("rhs")
        equality_comparison = SequenceTree.new([lhs, equals1, rhs], input, 0, 17)

        equality_comparison.descendants.should eq([lhs, seq_range1, t1, dots1, t2, equals1, rhs, seq_range2, t3, dots2, t4])
        equality_comparison.self_and_descendants.should eq([equality_comparison, lhs, seq_range1, t1, dots1, t2, equals1, rhs, seq_range2, t3, dots2, t4])
      end
    end

    describe "local_captures" do
      it "includes labels of any direct children" do
        input = "123456"
        t1 = TerminalTree.new(input[0..2], input, 0, 2).label("first")
        t2 = TerminalTree.new(input[3..5], input, 3, 5).label("last")
        s1 = SequenceTree.new([t1, t2] of ParseTree, input, 0, 5)

        s1.local_captures.should eq({"first" => [t1], "last" => [t2]})
      end

      it "aggregates captures with reused labels" do
        input = "123456"
        t1 = TerminalTree.new(input[0..2], input, 0, 2).label("nums")
        t2 = TerminalTree.new(input[3..5], input, 3, 5).label("nums")
        s1 = SequenceTree.new([t1, t2] of ParseTree, input, 0, 5)

        s1.local_captures.should eq({"nums" => [t1, t2]})
      end
    end

    describe "captures" do
      it "includes labels of any direct children" do
        input = "123456"
        t1 = TerminalTree.new(input[0..2], input, 0, 2).label("first")
        t2 = TerminalTree.new(input[3..5], input, 3, 5).label("last")
        s1 = SequenceTree.new([t1, t2] of ParseTree, input, 0, 5)

        s1.captures.should eq({"first" => [t1], "last" => [t2]})
      end

      it "aggregates captures with reused labels" do
        input = "123456"
        t1 = TerminalTree.new(input[0..2], input, 0, 2).label("nums")
        t2 = TerminalTree.new(input[3..5], input, 3, 5).label("nums")
        s1 = SequenceTree.new([t1, t2] of ParseTree, input, 0, 5)

        s1.captures.should eq({"nums" => [t1, t2]})
      end

      it "includes any labels of descendant children up to but excluding descendant apply tree nodes" do
        # equality_comparison = lhs=range "==" rhs=range
        # range = nums:[0-9]+ ".." nums:[0-9]+
        input = "111..222==333..444"
        t1 = TerminalTree.new(input[0..2], input, 0, 2).label("nums")
        dots1  = TerminalTree.new(input[3..4], input, 3, 4)
        t2 = TerminalTree.new(input[5..7], input, 5, 7).label("nums")
        equals1  = TerminalTree.new(input[8..9], input, 8, 9)
        t3 = TerminalTree.new(input[10..12], input, 10, 12).label("nums")
        dots2  = TerminalTree.new(input[13..14], input, 13, 14)
        t4 = TerminalTree.new(input[15..17], input, 15, 17).label("nums")
        seq_range1 = SequenceTree.new([t1, dots1, t2] of ParseTree, input, 0, 7)
        seq_range2 = SequenceTree.new([t3, dots2, t4] of ParseTree, input, 10, 17)
        lhs = ApplyTree.new(seq_range1, "range", input, 0, 7).label("lhs")
        rhs = ApplyTree.new(seq_range2, "range", input, 10, 17).label("rhs")
        equality_comparison = SequenceTree.new([lhs, equals1, rhs], input, 0, 17)

        equality_comparison.captures.should eq({"lhs" => [lhs], "rhs" => [rhs]})
        seq_range1.captures.should eq({"nums" => [t1, t2]})
        seq_range2.captures.should eq({"nums" => [t3, t4]})
      end
    end
  end

  describe "visitor" do
    it "works" do
      # e = e1=e - e2=e -- subtract
      #   | exprs+=e ("+" exprs+=e)* -- add
      #   | num         -- num
      # num = [0-9]+
      e = choice(
        seq(apply("e").label("e1"), term("-"), apply("e").label("e2")).label("subtract"), 
        seq(apply("e").label("exprs"), star(seq(term("+"), apply("e").label("exprs")))).label("add"), 
        apply("num").label("num")
      )
      num = plus(range('0'..'9'))
      m1 = Matcher.new.add_rule("e", e).add_rule("num", num)
      parse_tree = m1.match("1-2+10-3+10", "e")

      eval = Visitor(Int32).new
      
      eval.on("e_subtract") do |ctx|
        ctx.capture("e1").visit(eval) - ctx.capture("e2").visit(eval)
      end
      eval.on("e_add") do |ctx|
        ctx.captures("exprs").map(&.visit(eval)).sum
      end
      eval.on("e_num") do |ctx|
        ctx.capture("num").visit(eval)
      end

      eval.on("num") do |ctx|
        ctx.text.to_i
      end

      parse_tree.should_not be_nil
      raise "boom!" unless parse_tree
      eval.visit(parse_tree).should eq(16)
    end
  end

end
