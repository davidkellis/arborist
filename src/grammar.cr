require "./arborist"

module Arborist
  class Grammar
    module Rules
      include Arborist::DSL

      # grammar rules taken from:
      # https://github.com/harc/ohm/blob/master/src/ohm-grammar.ohm
      # https://github.com/harc/ohm/blob/8202eff3723cfa26522134e7b003cf31ab5de445/src/Grammar.js#L293
      # https://github.com/harc/ohm/blob/master/doc/syntax-reference.md#built-in-rules
      # https://github.com/harc/ohm/blob/master/src/built-in-rules.ohm

      Grammar = star(apply("Rule"))

      Rule = seq(apply("ident"), opt(apply("ruleDescr")), term("="), apply("RuleBody")).label("define")

      RuleBody = seq(opt(term("|")), apply("TopLevelTerm"), star(seq(term("|"), apply("TopLevelTerm"))))

      TopLevelTerm = choice(seq(apply("Seq"), apply("caseName")).label("inline"),
                            apply("Seq"))

      Alt = seq(apply("Seq"), star(seq(term("|"), apply("Seq"))))

      Seq = star(apply("Iter"))

      Iter = choice(
        seq(apply("Pred"), term("*")).label("star"),
        seq(apply("Pred"), term("+")).label("plus"),
        seq(apply("Pred"), term("?")).label("opt"),
        apply("Pred")
      )

      Pred = choice(
        seq(term("~"), apply("Base")).label("not"),
        seq(term("&"), apply("Base")).label("lookahead"),
        apply("Base"),
      )

      Base = choice(
        seq(apply("ident"), neg(seq(opt(apply("ruleDescr")), term("=")))).label("application"),
        seq(apply("oneCharTerminal"), term(".."), apply("oneCharTerminal")).label("range"),
        apply("terminal").label("terminal"),
        seq(term("("), apply("Alt"), term(")")).label("paren")
      )

      RuleDescr = seq(term("("), apply("ruleDescrText"), term(")"))

      RuleDescrText = star(seq(neg(term(")")), dot))

      CaseName = seq(
        term("--"),
        star(seq(neg(term("\n")), apply("space"))),
        apply("name"),
        star(seq(neg(term("\n")), apply("space"))),
        choice(term("\n"), pos("}"))
      )

      Name = seq(apply("nameFirst"), star(apply("nameRest")))

      NameFirst = choice(term("_"), apply("letter"))

      NameRest = choice(term("_"), apply("alnum"))

      Ident = apply("name")

      Terminal = seq(
        term("\""),
        star(apply("terminalChar")),
        term("\"")
      )

      OneCharTerminal = seq(
        term("\""),
        apply("terminalChar"),
        term("\"")
      )

      TerminalChar = choice(
        apply("escapeChar"),
        seq(neg(term("\\")), neg(term("\"")), neg(term("\n")), dot)
      )

      EscapeChar = choice(
        term("\\\\").label("backslash"),
        term("\\\"").label("doubleQuote"),
        term("\\\'").label("singleQuote"),
        term("\\b").label("backspace"),
        term("\\n").label("lineFeed"),
        term("\\r").label("carriageReturn"),
        term("\\t").label("tab"),
        seq(term("\\u"), apply("hexDigit"), apply("hexDigit"), apply("hexDigit"), apply("hexDigit")).label("unicodeEscape"),
        seq(term("\\x"), apply("hexDigit"), apply("hexDigit")).label("hexEscape"),
      )

      Space = choice(
        range('\u0000'..' '),
        apply("comment")
      )

      Comment = choice(
        seq(term("//"), star(seq(neg(term("\n")), dot)), term("\n")).label("singleLine"),
        seq(term("/*"), star(seq(neg(term("*/")), dot)), term("*/")).label("multiLine"),
      )

      Tokens = star(apply("token"))

      Token = choice(
        apply("caseName"),
        apply("comment"),
        apply("ident"),
        apply("operator"),
        apply("punctuation"),
        apply("terminal"),
        dot
      )

      Operator = choice(
        term("="),
        term("*"),
        term("+"),
        term("?"),
        term("~"),
        term("&")
      )

      Punctuation = choice(
        term("<"),
        term(">"),
        term(","),
        term("--")
      )

      Parser = Matcher.new.add_rule("Grammar", Grammar)
                          .add_rule("Rule", Rule)
                          .add_rule("RuleBody", RuleBody)
                          .add_rule("TopLevelTerm", TopLevelTerm)
                          .add_rule("Alt", Alt)
                          .add_rule("Seq", Seq)
                          .add_rule("Iter", Iter)
                          .add_rule("Pred", Pred)
                          .add_rule("Base", Base)
                          .add_rule("ruleDescr", RuleDescr)
                          .add_rule("ruleDescrText", RuleDescrText)
                          .add_rule("caseName", CaseName)
                          .add_rule("name", Name)
                          .add_rule("nameFirst", NameFirst)
                          .add_rule("nameRest", NameRest)
                          .add_rule("ident", Ident)
                          .add_rule("terminal", Terminal)
                          .add_rule("oneCharTerminal", OneCharTerminal)
                          .add_rule("terminalChar", TerminalChar)
                          .add_rule("escapeChar", EscapeChar)
                          .add_rule("space", Space)
                          .add_rule("comment", Comment)
                          .add_rule("tokens", Tokens)
                          .add_rule("token", Token)
                          .add_rule("operator", Operator)
                          .add_rule("punctuation", Punctuation)
    end

    GrammarVisitor = Visitor.new
      
    GrammarVisitor.on("e") do |alt_node|
      case alt_node.label
      when "subtract"
      when "add"
      when ""
      end
    end



    @matcher : Matcher?

    def initialize
      @matcher = nil
    end

    def initialize(grammar_file_path : String)
      initialize()
      load_grammar_file(grammar_file_path)
    end

    def load_grammar_file(path : String)
      grammar_defn = File.read(path)
      load_grammar(grammar_defn)
    end

    def load_grammar(grammar_defn : String)
      @matcher = build_matcher(grammar_defn)
    end

    def build_matcher(grammar_defn : String) : Matcher
      parse_tree = Rules::Parser.match(grammar_defn, "Grammar")
      GrammarVisitor.walk(parse_tree)
    end

    def parse_file(path : String)
      file_contents = File.read(path)
      parse(file_contents)
    end

    def parse(str : String) : ParseTree?
      if matcher = @matcher
      end
    end
  end
end