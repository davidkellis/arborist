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

  describe "mutex alternation operator" do
    it "matches any string in the set" do
      a = alt("a", "b", "c")
      m = Matcher.new.add_rule("a", a)

      m.match("").try(&.syntax_tree).should be_nil
      m.match("a").try(&.syntax_tree).should eq("a")
      m.match("b").try(&.syntax_tree).should eq("b")
      m.match("c").try(&.syntax_tree).should eq("c")
      m.match("d").try(&.syntax_tree).should be_nil
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
      r1 = seq(neg(term("abc")), seq(dot, dot, dot))   # !"abc" /.../
      m1 = Matcher.new.add_rule("start", r1)

      m1.match("abc").try(&.syntax_tree).should be_nil
      m1.match("xyz").try(&.syntax_tree).should eq [["x", "y", "z"]]
    end

    it "can be stacked to ensure multiple things do not match" do
      r1 = seq(neg(term("abc")), neg(term("xyz")), seq(dot, dot, dot))   # !"abc" !"xyz" /.../
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
      num = plus(range('0'..'9'))                           # num -> [0-9]+
      m1 = Matcher.new.add_rule("expr", expr).add_rule("num", num)

      m1.match("1-2-3", "expr").should be_nil
    end

    it "allows rules that are left-recursion and not right-recursive" do
      expr = choice(seq(apply("expr"), term("-"), apply("num")), apply("num"))    # expr -> expr - num | num
      num = plus(range('0'..'9'))                                                 # num -> [0-9]+
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
      num = plus(range('0'..'9'))                                                 # num -> [0-9]+
      m1 = Matcher.new.add_rule("expr", expr).add_rule("num", num)

      m1.match("1-2-3", "expr").try(&.syntax_tree).should eq [["1"], "-", [["2"], "-", ["3"]]]   # should parse as (1-(2-(3))
    end

    it "allows rules that are left-recursive and simultaneously recursive in a second point, but not right-recursive" do
      e = choice(seq(apply("e"), term("-"), apply("e"), term("m")), term("5"))    # e -> e - e "m" | 5
      m1 = Matcher.new.add_rule("e", e)

      m1.match("5-5m-5m", "e").try(&.syntax_tree).should eq [["5", "-", "5", "m"], "-", "5", "m"]   # should parse as (((5)-5m)-5m)
    end

    it "allows rules that are left and right recursive" do
      e = choice(seq(apply("e"), term("-"), apply("e")), term("5"))    # e -> e - e | 5
      m1 = Matcher.new.add_rule("e", e)

      # Arborist::GlobalDebug.enable!
      m1.match("5-5-5", "e").try(&.syntax_tree).should eq [["5", "-", "5"], "-", "5"]   # should parse as (((5)-5)-5)
      # Arborist::GlobalDebug.disable!
      m1.match("5-5-5-5-5", "e").try(&.syntax_tree).should eq [[[["5", "-", "5"], "-", "5"], "-", "5"], "-", "5"]   # should parse as (((((5)-5)-5)-5)-5)
    end

    it "correctly parses e -> e + m / m ; m -> m * m / num" do
      # e -> e + m / m
      # m -> m * m / num
      # num -> [0-9]+
      e = choice(seq(apply("e"), term("+"), apply("m")),
                 apply("m") )
      m = choice(seq(apply("m"), term("*"), apply("m")),
                 apply("num") )
      num = plus(range('0'..'9'))
      m1 = Matcher.new.add_rule("e", e).add_rule("m", m).add_rule("num", num)

      # 1+2*3+4*5 should parse as ((1+(2*3))+(4*5))
      m1.match("1+2*3+4*5", "e").try(&.syntax_tree).should eq [[["1"], "+", [["2"], "*", ["3"]]], "+", [["4"], "*", ["5"]]]
    end

    it "correctly parses e -> e + e | e * e | num" do
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

    pending "never matches any phrase for the rule: a = !a 'b' ; see https://github.com/harc/ohm/issues/120" do
      # a -> !a "b"
      # This rule is paradoxical - a is defined to recognize a string that is not prefixed with itself, therefore, it can't
      # recognize any string; however, if a can't recognize anything, then by failing to recognize anything, `!a` succeeds,
      # which then allows `a` to match the "b" terminal when the input string is "b". But then if a can recognize "b", 
      # then `!a` should prevent `a` from matching the "b" terminal, resulting in `a` not being able to recognize anything.
      # So, which is it?
      # My gut feeling is that it should not match anything.

      a = seq(neg(apply("a")), term("b"))
      m1 = Matcher.new.add_rule("a", a)

      m1.match("").should be_nil
      m1.match("b").try(&.syntax_tree).should be_nil
      m1.match("bb").try(&.syntax_tree).should be_nil
      m1.match("a").try(&.syntax_tree).should be_nil
    end

    describe "handles left recursion edge cases" do
      it "correctly recognizes `a(b,c(d,e))` with grammar: e -> e '(' e ',' e ')' / a-z" do
        e = choice(
          seq(apply("e"), term("("), apply("e"), term(","), apply("e"), term(")")),
          plus(range('a'..'z'))
        )
        m = Matcher.new.add_rule("e", e)

        # Arborist::GlobalDebug.enable!
        # pt = m.match("foo", "e")
        # Arborist::GlobalDebug.disable!
        # m.print_match_failure_error unless pt
        # pt.try(&.simple_s_exp).should_not be_nil

        m.match("foo", "e").try(&.simple_s_exp).should eq "(e f o o)"
        m.match("foo(abc,qux)", "e").try(&.simple_s_exp).should eq "(e (e f o o) ( (e a b c) , (e q u x) ))"
        # Arborist::GlobalDebug.enable!
        m.match("a(b(c,d),e)", "e").try(&.simple_s_exp).should eq "(e (e a) ( (e (e b) ( (e c) , (e d) )) , (e e) ))"
        # Arborist::GlobalDebug.disable!
        m.match("a(b,c(d,e))", "e").try(&.simple_s_exp).should eq "(e (e a) ( (e b) , (e (e c) ( (e d) , (e e) )) ))"
        m.match("a(b(c,d),f(g,h))", "e").try(&.simple_s_exp).should eq "(e (e a) ( (e (e b) ( (e c) , (e d) )) , (e (e f) ( (e g) , (e h) )) ))"
      end

      it "correctly recognizes multiple left recursive alternatives" do
        e = choice(
          seq(apply("e"), term("."), apply("e")),
          seq(apply("e"), term("("), star(seq(apply("e"), opt(term(","))) ), term(")")),
          plus(range('a'..'z')),
          plus(range('0'..'9'))
        )
        m = Matcher.new.add_rule("e", e)

        # Arborist::GlobalDebug.enable!
        # pt = m.match("foo", "e")
        # Arborist::GlobalDebug.disable!
        # m.print_match_failure_error unless pt
        # pt.try(&.simple_s_exp).should_not be_nil

        (pt = m.match("foo", "e")).try(&.simple_s_exp).should_not be_nil
        (pt = m.match("foo()", "e")).try(&.simple_s_exp).should_not be_nil
        (pt = m.match("foo().bar", "e")).try(&.simple_s_exp).should_not be_nil
        (pt = m.match("foo().bar.baz(123)", "e")).try(&.simple_s_exp).should_not be_nil
        (pt = m.match("foo().bar.baz(123,qux)", "e")).try(&.simple_s_exp).should_not be_nil
        # Arborist::GlobalDebug.enable!
        (pt = m.match("a(b,c(d))", "e")).try(&.simple_s_exp).should_not be_nil
        # Arborist::GlobalDebug.disable!
        (pt = m.match("foo().bar.baz(123,qux(456))", "e")).try(&.simple_s_exp).should_not be_nil
        (pt = m.match("foo().bar.baz(123,qux(456).quux(789).oof)", "e")).try(&.simple_s_exp).should_not be_nil
      end

      it "correctly recognizes `(1+2)` with grammar: e -> e + e / '(' e ')' / 1 / 2" do
        e = choice(
          seq(apply("e"), term("+"), apply("e")),
          seq(term("("), apply("e"), term(")")),
          term("1"),
          term("2")
        )
        m = Matcher.new.add_rule("e", e)

        # (1+2)
        # Arborist::GlobalDebug.enable!
        m.match("(1+2)", "e").try(&.syntax_tree).should eq ["(", ["1", "+", "2"], ")"]
        # Arborist::GlobalDebug.disable!
      end

      it "correctly recognizes `((2-1-1)-1-2-1-(2-2-1))` with grammar: e -> e - e / '(' e ')' / 1 / 2" do
        e = choice(
          seq(apply("e"), term("-"), apply("e")),
          seq(term("("), apply("e"), term(")")),
          term("1"),
          term("2")
        )
        m = Matcher.new.add_rule("e", e)

        # ((2-1-1)-1-2-1-(2-2-1))
        m.match("((2-1-1)-1-2-1-(2-2-1))", "e").try(&.syntax_tree).should eq \
          ["(", 
            [
              [ [ [ ["(", [ ["2", "-", "1"], "-", "1"], ")"],
                    "-",
                    "1" ],
                  "-", 
                  "2" ],
                "-",
                "1" ],
              "-",
              ["(", [ ["2", "-", "2"], "-", "1"], ")"]
            ],
          ")"]
      end

      it "correctly recognizes `a,b=1,1+2+1` with grammar: e -> id, id = e, e / add / 1 / 2 ; add -> e + e ; id -> a / b" do
        # e -> id, id = e, e / add / 1 / 2
        # add -> e + e
        # id -> "a" / "b"
        e = choice(
          seq(apply("id"), term(","), apply("id"), term("="), apply("e"), term(","), apply("e")),
          apply("add"),
          term("1"),
          term("2")
        )
        add = choice(
          seq(apply("e"), term("+"), apply("e"))
        )
        id = choice(
          term("a"),
          term("b")
        )
        m = Matcher.new.add_rule("e", e).add_rule("add", add).add_rule("id", id)

        # a,b=1,1+2+1
        # Arborist::GlobalDebug.enable!
        m.match("a,b=1,1+2+1", "e").try(&.simple_s_exp).should eq "(e (id a) , (id b) = (e 1) , (e (add (e (add (e 1) + (e 2))) + (e 1))))"
        # Arborist::GlobalDebug.disable!
      end

    end

    # See https://github.com/PhilippeSigaud/Pegged/wiki/Left-Recursion
    # and https://github.com/PhilippeSigaud/Pegged/blob/d091b9f5b7dc1401a989b262ca30b113841b48cc/pegged/grammar.d
    describe "handles various kinds of left recursion that the Pegged library handles" do
      it "supports direct left recursion" do
        # Left:
        # E <- E '+n' / 'n'
        e = choice(
          seq(
            apply("e"),
            term("+n")
          ),
          term("n")
        )
        m1 = Matcher.new.add_rule("e", e)

        # n+n+n should parse as (((n) + n) + n)
        m1.match("n+n+n", "e").try(&.syntax_tree).should eq [["n", "+n"], "+n"]
      end

      it "supports hidden left recursion" do
        # E <- F? E '+n' / 'n'
        e = choice(
          seq(
            opt(apply("f")),
            apply("e"),
            term("+n")
          ),
          term("n")
        )
        f = term("foo")
        m1 = Matcher.new.add_rule("e", e).add_rule("f", f)

        # n+n+n should parse as (((n) + n) + n)
        m1.match("n+n+n", "e").try(&.syntax_tree).should eq [["n", "+n"], "+n"]
      end

      it "supports obscured hidden left recursion" do
        # E <- F E '+n' / 'n'
        # F <- A B C / D*
        e = choice(
          seq(
            apply("f"),
            apply("e"),
            term("+n")
          ),
          term("n")
        )
        f = choice(
          seq(
            apply("a"),
            apply("b"),
            apply("c")
          ),
          star(apply("d"))
        )
        a = term("a")
        b = term("b")
        c = term("c")
        d = term("d")
        m1 = Matcher.new.add_rule("e", e).add_rule("f", f).add_rule("a", a).add_rule("b", b).add_rule("c", c).add_rule("d", d)

        # n+n+n should parse as (((n) + n) + n)
        m1.match("n+n+n", "e").try(&.syntax_tree).should eq [[] of String, [[] of String, "n", "+n"], "+n"]
      end

      it "supports simple indirect left recursion" do
        # E <- F '+n' / 'n'
        # F <- E
        e = choice(
          seq(
            apply("f"),
            term("+n")
          ),
          term("n")
        )
        f = apply("e")
        m5 = Matcher.new.
          add_rule("e", e).
          add_rule("f", f)

        # n+n+n should parse as (((n) + n) + n)
        # Arborist::GlobalDebug.enable!
        m5.match("n+n+n", "e").try(&.syntax_tree).should eq [["n", "+n"], "+n"]
        # Arborist::GlobalDebug.disable!
      end

      it "supports indirect left recursion" do
        # E <- F '+n' / 'n'
        # F <- G H / J
        # J <- K / E L?
        e = choice(
          seq(
            apply("f"),
            term("+n")
          ),
          term("n")
        )
        f = choice(
          seq(
            apply("g"),
            apply("h")
          ),
          apply("j")
        )
        j = choice(
          apply("k"),
          seq(apply("e"), opt(apply("l")))
        )
        g = term("g")
        h = term("h")
        k = term("k")
        l = term("l")
        m1 = Matcher.new.
          add_rule("e", e).
          add_rule("f", f).
          add_rule("g", g).
          add_rule("h", h).
          add_rule("j", j).
          add_rule("k", k).
          add_rule("l", l)

        # n+n+n should parse as (((n) + n) + n)
        m1.match("n+n+n", "e").try(&.syntax_tree).should eq [[[["n"], "+n"]], "+n"]
      end

      it "supports mutually left recursive rules" do
        # L <- P '.x' / 'x'
        # P <- P '(n)' / L
        l = choice(
          seq(
            apply("p"),
            term(".x")
          ),
          term("x")
        )
        p = choice(
          seq(
            apply("p"),
            term("(n)")
          ),
          apply("l")
        )
        m1 = Matcher.new.
          add_rule("l", l).
          add_rule("p", p)

        # Per http://www.inf.puc-rio.br/~roberto/docs/sblp2012.pdf:
        # This grammar generatesxandxfollowed by any number of (n) or.x, as longas it ends with.x. 
        # An l-value is a prefix expression followed by a field access, ora single variable, and a prefix expression 
        # is a prefix expression followed by anoperand, denoting a function call, or a valid l-value. 
        # In the parse trees for thisgrammar each (n) or.xassociates to the left.
        m1.match("x(n)(n).x(n).x", "l").try(&.syntax_tree).should eq [[[[["x", "(n)"], "(n)"], ".x"], "(n)"], ".x"]
      end

      it "supports interlocking cycles of indirect left-recursion" do
        # E <- F 'n' / 'n'
        # F <- E '+' I* / G '-'
        # G <- H 'm' / E
        # H <- G 'l'
        # I <- '(' A+ ')'
        # A <- 'a'
        e = choice(
          seq(
            apply("f"),
            term("n")
          ),
          term("n")
        )
        f = choice(
          seq(
            apply("e"),
            term("+"),
            star(apply("i"))
          ),
          seq(
            apply("g"),
            term("-")
          )
        )
        g = choice(
          seq(
            apply("h"),
            term("m")
          ),
          apply("e")
        )
        h = seq(
          apply("g"),
          term("l")
        )
        i = seq(
          term("("),
          plus(apply("a")),
          term(")")
        )
        a = term("a")
        m1 = Matcher.new.
          add_rule("e", e).
          add_rule("f", f).
          add_rule("g", g).
          add_rule("h", h).
          add_rule("i", i).
          add_rule("a", a)

        m1.match("nlm-n+(aaa)n", "e").try(&.syntax_tree).should eq [[[[[["n", "l"], "m"], "-"], "n"], "+", [["(", ["a", "a", "a"], ")"]]], "n"]
      end
    end

    # See https://github.com/norswap/autumn/blob/master/doc/A6-left-recursion-associativity.md
    # and https://github.com/norswap/autumn/blob/master/test/TestParsers.java
    describe "handles various kinds of left recursion that the Autumn library handles" do
      it "supports simple left recursion" do
        # A -> Aa | a
        a = choice(
          seq(
            apply("a"),
            term("a")
          ),
          term("a")
        )
        m = Matcher.new.add_rule("a", a)

        m.match("a", "a").try(&.syntax_tree).should eq "a"
        m.match("aa", "a").try(&.syntax_tree).should eq ["a", "a"]
        m.match("aaa", "a").try(&.syntax_tree).should eq [["a", "a"], "a"]
        m.match("aaaa", "a").try(&.syntax_tree).should eq [[["a", "a"], "a"], "a"]
      end

      it "supports nested left recursion" do
        # A -> Aa | a
        # B -> Bb | A

        # this test grammar is also implemented in arborist/antlr4/AB1.g4
        # test it like:
        # arborist/antlr4 ❯ antlr4 AB1.g4
        # arborist/antlr4 ❯ javac AB1*.java
        # arborist/antlr4 ❯ echo abbb | grun AB1 b -tree
        # line 1:4 token recognition error at: '\n'
        # (b (b (b (b (a a)) b) b) b)

        a = choice(
          seq(
            apply("a"),
            term("a")
          ),
          term("a")
        )
        b = choice(
          seq(
            apply("b"),
            term("b")
          ),
          apply("a")
        )
        m = Matcher.new.add_rule("a", a).add_rule("b", b)

        m.match("ab", "b").try(&.syntax_tree).should eq ["a", "b"]                        # antlr4: (b (b (a a)) b)
        m.match("ab", "b").try(&.simple_s_exp).should eq "(b (b (a a)) b)"
        m.match("aaab", "b").try(&.syntax_tree).should eq [[["a", "a"], "a"], "b"]        # antlr4: (b (b (a (a (a a) a) a)) b)
        m.match("aaab", "b").try(&.simple_s_exp).should eq "(b (b (a (a (a a) a) a)) b)"
        m.match("abbb", "b").try(&.syntax_tree).should eq [[["a", "b"], "b"], "b"]        # antlr4: (b (b (b (b (a a)) b) b) b)
        m.match("abbb", "b").try(&.simple_s_exp).should eq "(b (b (b (b (a a)) b) b) b)"
        m.match("aaabbb", "b").try(&.syntax_tree).should eq [[[[["a", "a"], "a"], "b"], "b"], "b"]    # antlr4: (b (b (b (b (a (a (a a) a) a)) b) b) b)
        m.match("aaabbb", "b").try(&.simple_s_exp).should eq "(b (b (b (b (a (a (a a) a) a)) b) b) b)"
      end

      it "supports simple left- and right- recursive rules (and is left-associative)" do
        # A -> AA | a
        a = choice(
          seq(
            apply("a"),
            apply("a")
          ),
          term("a")
        )
        m = Matcher.new.add_rule("a", a)

        m.match("a", "a").try(&.syntax_tree).should eq "a"
        m.match("aa", "a").try(&.syntax_tree).should eq ["a", "a"]
        m.match("aaa", "a").try(&.syntax_tree).should eq [["a", "a"], "a"]
        m.match("aaaa", "a").try(&.syntax_tree).should eq [[["a", "a"], "a"], "a"]
      end

      it "supports left- and right-recursion + right-recursion; producing the left-most derivation" do
        # A -> AA | bA | a

        # this test grammar is also implemented in arborist/antlr4/A1.g4
        # test it like:
        # arborist/antlr4 ❯ antlr4 A1.g4
        # arborist/antlr4 ❯ javac A1*.java
        # arborist/antlr4 ❯ echo baa | grun A1 a -tree
        # line 1:3 token recognition error at: '\n'
        # line 2:0 no viable alternative at input '<EOF>'
        # (a b (a (a a) (a a)))

        a = choice(
          seq(
            apply("a"),
            apply("a")
          ),
          seq(
            term("b"),
            apply("a")
          ),
          term("a")
        )
        m = Matcher.new.add_rule("a", a)

        m.match("a", "a").try(&.syntax_tree).should eq "a"
        m.match("a", "a").try(&.simple_s_exp).should eq "(a a)"
        m.match("aa", "a").try(&.syntax_tree).should eq ["a", "a"]
        m.match("aaa", "a").try(&.syntax_tree).should eq [["a", "a"], "a"]
        m.match("aaaa", "a").try(&.syntax_tree).should eq [[["a", "a"], "a"], "a"]    # parses like Antlr4: (a (a (a (a a) (a a)) (a a)) (a a))
        m.match("aaaa", "a").try(&.simple_s_exp).should eq "(a (a (a (a a) (a a)) (a a)) (a a))"
        m.match("ba", "a").try(&.syntax_tree).should eq ["b", "a"]
        # Arborist::GlobalDebug.enable!
        m.match("baa", "a").try(&.syntax_tree).should eq ["b", ["a", "a"]]            # parses like Antlr4: (a b (a (a a) (a a)))
        m.match("baa", "a").try(&.simple_s_exp).should eq "(a b (a (a a) (a a)))"
        # Arborist::GlobalDebug.disable!
        m.match("bba", "a").try(&.syntax_tree).should eq ["b", ["b", "a"]]
        m.match("bbaa", "a").try(&.syntax_tree).should eq ["b", ["b", ["a", "a"]]]    # parses like Antlr4: (a b (a b (a (a a) (a a))))
        m.match("bbaa", "a").try(&.simple_s_exp).should eq "(a b (a b (a (a a) (a a))))"
        m.match("b", "a").try(&.syntax_tree).should be_nil
        m.match("", "a").try(&.syntax_tree).should be_nil
      end

      it "supports separated left- and right-recursion (right-recursion first); produces a right-associative derivation" do
        # A -> aA | Aa | a

        # this test grammar is also implemented in arborist/antlr4/A2.g4
        # test it like:
        # arborist/antlr4 ❯ antlr4 A2.g4
        # arborist/antlr4 ❯ javac A2*.java
        # arborist/antlr4 ❯ echo aaaa | grun A2 a -tree
        # line 1:4 token recognition error at: '\n'
        # line 2:0 no viable alternative at input '<EOF>'
        # (a a (a a (a a (a a))))
        
        a = choice(
          seq(
            term("a"),
            apply("a")
          ),
          seq(
            apply("a"),
            term("a")
          ),
          term("a")
        )
        m = Matcher.new.add_rule("a", a)

        m.match("a", "a").try(&.syntax_tree).should eq "a"
        m.match("aa", "a").try(&.syntax_tree).should eq ["a", "a"]
        m.match("aaa", "a").try(&.syntax_tree).should eq ["a", ["a", "a"]]
        m.match("aaaa", "a").try(&.syntax_tree).should eq ["a", ["a", ["a", "a"]]]
        m.match("aaaa", "a").try(&.simple_s_exp).should eq "(a a (a a (a a (a a))))"
        m.match("b", "a").try(&.syntax_tree).should be_nil
        m.match("", "a").try(&.syntax_tree).should be_nil
      end

      # This test needs some explanation:
      # 'aaa' should be parsed such that the parse tree reflects a right-associative derivation - (a, (a, a)) - because
      # even though the first term is left-recursive, a left-associative derivation is impossible due to the rules
      # of the PEG formalism, specifically, that ordered choice prioritizes the left-most alternatives first, and even though
      # the `Aa` alternative is listed first, in order for it to match, it must match *in conjunction* with the third
      # alternative, `a`, which is impossible because the second alternative, `aA`, can match the full string with a right
      # associative derivation before the third alternative is ever tried. Therefore, even though intution may suggest that
      # A -> Aa / aA / a should parse the string 'aaa' as ((a, a), a), that cannot happen as long as `aA` precedes `a` in
      # the sequence of alternatives.
      it "supports separated left- and right-recursion (left-recursion first); produces a right-associative derivation" do
        # A -> Aa / aA / a

        # this test grammar is also implemented in arborist/antlr4/A3.g4
        # test it like:
        # arborist/antlr4 ❯ antlr4 A3.g4
        # arborist/antlr4 ❯ javac A3*.java
        # arborist/antlr4 ❯ echo aaaa | grun A3 a -tree
        # line 1:4 token recognition error at: '\n'
        # line 2:0 no viable alternative at input '<EOF>'
        # (a a (a a (a a (a a) <EOF>) <EOF>) <EOF>)

        a = choice(
          seq(
            apply("a"),
            term("a")
          ),
          seq(
            term("a"),
            apply("a")
          ),
          term("a")
        )
        m = Matcher.new.add_rule("a", a)

        m.match("a", "a").try(&.syntax_tree).should eq "a"
        m.match("aa", "a").try(&.syntax_tree).should eq ["a", "a"]
        # Arborist::GlobalDebug.enable!
        m.match("aaa", "a").try(&.syntax_tree).should eq ["a", ["a", "a"]]
        m.match("aaa", "a").try(&.simple_s_exp).should eq "(a a (a a (a a)))"
        # Arborist::GlobalDebug.disable!
        m.match("aaaa", "a").try(&.syntax_tree).should eq ["a", ["a", ["a", "a"]]]
        m.match("aaaa", "a").try(&.simple_s_exp).should eq "(a a (a a (a a (a a))))"
        m.match("b", "a").try(&.syntax_tree).should be_nil
        m.match("", "a").try(&.syntax_tree).should be_nil
      end

    end

  end

  describe "parse tree" do
    describe "parent/children relationship" do
      it "can be navigated via parent field" do
        input = "123456"
        t1 = TerminalTree.new(input[0..2], CharArray.new(input), 0, 2)
        t2 = TerminalTree.new(input[3..5], CharArray.new(input), 3, 5)
        s1 = SequenceTree.new([t1, t2] of ParseTree, CharArray.new(input), 0, 5)

        s1.recursively_populate_parents

        t1.parent.should eq(s1)
        t2.parent.should eq(s1)
        s1.parent.should be_nil
      end

      it "lists descendants in an order derived from a pre-order traversal of the nodes" do
        # equality_comparison = lhs=range "==" rhs=range
        # range = nums:[0-9]+ ".." nums:[0-9]+
        input = "111..222==333..444"
        char_array = CharArray.new(input)
        t1 = TerminalTree.new(input[0..2], char_array, 0, 2).label("nums")
        dots1  = TerminalTree.new(input[3..4], char_array, 3, 4)
        t2 = TerminalTree.new(input[5..7], char_array, 5, 7).label("nums")
        equals1  = TerminalTree.new(input[8..9], char_array, 8, 9)
        t3 = TerminalTree.new(input[10..12], char_array, 10, 12).label("nums")
        dots2  = TerminalTree.new(input[13..14], char_array, 13, 14)
        t4 = TerminalTree.new(input[15..17], char_array, 15, 17).label("nums")
        seq_range1 = SequenceTree.new([t1, dots1, t2] of ParseTree, char_array, 0, 7)
        seq_range2 = SequenceTree.new([t3, dots2, t4] of ParseTree, char_array, 10, 17)
        lhs = ApplyTree.new(seq_range1, "range", char_array, 0, 7).label("lhs")
        rhs = ApplyTree.new(seq_range2, "range", char_array, 10, 17).label("rhs")
        equality_comparison = SequenceTree.new([lhs, equals1, rhs], char_array, 0, 17)

        equality_comparison.descendants.should eq([lhs, seq_range1, t1, dots1, t2, equals1, rhs, seq_range2, t3, dots2, t4])
        equality_comparison.self_and_descendants.should eq([equality_comparison, lhs, seq_range1, t1, dots1, t2, equals1, rhs, seq_range2, t3, dots2, t4])
      end
    end

    describe "local_captures" do
      it "includes labels of any direct children" do
        input = "123456"
        char_array = CharArray.new(input)
        t1 = TerminalTree.new(input[0..2], char_array, 0, 2).label("first")
        t2 = TerminalTree.new(input[3..5], char_array, 3, 5).label("last")
        s1 = SequenceTree.new([t1, t2] of ParseTree, char_array, 0, 5)

        s1.local_captures.should eq({"first" => [t1], "last" => [t2]})
      end

      it "aggregates captures with reused labels" do
        input = "123456"
        char_array = CharArray.new(input)
        t1 = TerminalTree.new(input[0..2], char_array, 0, 2).label("nums")
        t2 = TerminalTree.new(input[3..5], char_array, 3, 5).label("nums")
        s1 = SequenceTree.new([t1, t2] of ParseTree, char_array, 0, 5)

        s1.local_captures.should eq({"nums" => [t1, t2]})
      end
    end

    describe "captures" do
      it "includes labels of any direct children" do
        input = "123456"
        char_array = CharArray.new(input)
        t1 = TerminalTree.new(input[0..2], char_array, 0, 2).label("first")
        t2 = TerminalTree.new(input[3..5], char_array, 3, 5).label("last")
        s1 = SequenceTree.new([t1, t2] of ParseTree, char_array, 0, 5)

        s1.captures.should eq({"first" => [t1], "last" => [t2]})
      end

      it "aggregates captures with reused labels" do
        input = "123456"
        char_array = CharArray.new(input)
        t1 = TerminalTree.new(input[0..2], char_array, 0, 2).label("nums")
        t2 = TerminalTree.new(input[3..5], char_array, 3, 5).label("nums")
        s1 = SequenceTree.new([t1, t2] of ParseTree, char_array, 0, 5)

        s1.captures.should eq({"nums" => [t1, t2]})
      end

      it "includes any labels of descendant children up to but excluding descendant apply tree nodes" do
        # equality_comparison = lhs=range "==" rhs=range
        # range = nums:[0-9]+ ".." nums:[0-9]+
        input = "111..222==333..444"
        char_array = CharArray.new(input)
        t1 = TerminalTree.new(input[0..2], char_array, 0, 2).label("nums")
        dots1  = TerminalTree.new(input[3..4], char_array, 3, 4)
        t2 = TerminalTree.new(input[5..7], char_array, 5, 7).label("nums")
        equals1  = TerminalTree.new(input[8..9], char_array, 8, 9)
        t3 = TerminalTree.new(input[10..12], char_array, 10, 12).label("nums")
        dots2  = TerminalTree.new(input[13..14], char_array, 13, 14)
        t4 = TerminalTree.new(input[15..17], char_array, 15, 17).label("nums")
        seq_range1 = SequenceTree.new([t1, dots1, t2] of ParseTree, char_array, 0, 7)
        seq_range2 = SequenceTree.new([t3, dots2, t4] of ParseTree, char_array, 10, 17)
        lhs = ApplyTree.new(seq_range1, "range", char_array, 0, 7).label("lhs")
        rhs = ApplyTree.new(seq_range2, "range", char_array, 10, 17).label("rhs")
        equality_comparison = SequenceTree.new([lhs, equals1, rhs], char_array, 0, 17)

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
