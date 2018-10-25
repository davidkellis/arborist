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

      # Grammar
      #   = ident "{" Rule* "}"
      #   | Rule*
      Grammar = choice(
        seq(apply("ident"), term("{"), star(apply("Rule")), term("}")).label("named"),
        star(apply("Rule")).label("unnamed")
      )

      # Rule = ident "=" RuleBody
      Rule = seq(apply("ident"), term("="), apply("RuleBody")).label("define")

      RuleBody = seq(opt(term("|")), apply("TopLevelTerm"), star(seq(term("|"), apply("TopLevelTerm"))))

      TopLevelTerm = choice(
        seq(apply("Seq"), apply("caseName")).label("inline"),
        apply("Seq").label("seq")
      )

      Alt = seq(apply("Seq"), star(seq(term("|"), apply("Seq"))))

      Seq = star(apply("Pred"))

      Pred = choice(
        seq(term("~"), apply("Iter")).label("neg"),
        seq(term("&"), apply("Iter")).label("pos"),
        apply("Iter").label("iter"),
      )

      Iter = choice(
        seq(apply("Label"), term("*")).label("star"),
        seq(apply("Label"), term("+")).label("plus"),
        seq(apply("Label"), term("?")).label("opt"),
        apply("Label").label("label")
      )

      Label = choice(
        seq(apply("ident"), term("="), apply("Base")).label("label"),
        apply("Base").label("base")
      )

      # # Lex
      # #   = "#" Base  -- lex
      # #   | Base
      # Lex = choice(
      #   seq(term("#"), apply("Base")).label("lex"),
      #   apply("Base"),
      # )

      Base = choice(
        seq(apply("ident"), neg(seq(opt(apply("ruleDescr")), term("=")))).label("application"),
        seq(apply("oneCharTerminal"), term(".."), apply("oneCharTerminal")).label("range"),
        apply("terminal").label("terminal"),
        seq(term("("), apply("Alt"), term(")")).label("group"),
        term(".").label("dot")
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
        apply("escapeChar").label("escape"),
        seq(neg(term("\\")), neg(term("\"")), neg(term("\n")), dot.label("char")).label("char")
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

      # The `skip` rule is special, in that Syntactic rules (rules named with an uppercase first letter) will skip/ignore any number of
      # matches of the `skip` rule occurring immediately prior to or immediately following any of the terms that make up the rule body.
      # In other words, the rule `Foo = "bar" "baz"` would match on the string "  \n\tbar      \t\t\n\n    baz \t\n "", and the
      # whitespace in between the terms would be ignored.
      Skip = star(apply("space"))
      
      Space = choice(
        range('\u0000'..' '),
        apply("comment")
      )

      # comment
      #   = "//" (~"\n" dot)* "\n"  -- singleLine
      #   | "/*" (~"*/" dot)* "*/"  -- multiLine
      Comment = choice(
        seq(term("//"), star(seq(neg(term("\n")), dot)), term("\n")).label("singleLine"),
        seq(term("/*"), star(seq(neg(term("*/")), dot)), term("*/")).label("multiLine")
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

      HexDigit = choice(
        range('0'..'9'),
        range('A'..'F'),
        range('a'..'f')
      )

      GrammarParser = Matcher.new
        .add_rule("Grammar", Grammar)
        .add_rule("Rule", Rule)
        .add_rule("RuleBody", RuleBody)
        .add_rule("TopLevelTerm", TopLevelTerm)
        .add_rule("Alt", Alt)
        .add_rule("Seq", Seq)
        .add_rule("Pred", Pred)
        .add_rule("Iter", Iter)
        .add_rule("Label", Label)
        # .add_rule("Lex", Lex)
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
        .add_rule("skip", Skip)
        .add_rule("space", Space)
        .add_rule("comment", Comment)
        .add_rule("tokens", Tokens)
        .add_rule("token", Token)
        .add_rule("operator", Operator)
        .add_rule("punctuation", Punctuation)
        .add_rule("hexDigit", HexDigit)
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

    def build_matcher(grammar_defn : String) : Matcher?
      grammar_parse_tree = Rules::GrammarParser.match(grammar_defn, "Grammar")
      GrammarParserBuilder.new.build_grammar_parser(grammar_parse_tree) if grammar_parse_tree
    end

    def parse_file(path : String)
      file_contents = File.read(path)
      parse(file_contents)
    end

    def parse(input_str : String) : ParseTree?
      if matcher = @matcher
        matcher.match(input_str)
      end
    end


    class GrammarParserBuilder
      include Arborist::DSL

      # @visitor : Visitor(Object)
      @visitor : Visitor(Matcher | Expr)
      @matcher : Matcher

      def initialize
        # @visitor = Visitor(Object).new
        @visitor = Visitor(Matcher | Expr).new
        @matcher = Matcher.new
      end

      def build_visitor
        # @visitor = Visitor(Object).new
        @visitor = Visitor(Matcher | Expr | String).new
        @matcher = Matcher.new

        # @visitor.on("Grammar_named") do |grammar|
        #   grammar.captures("Rule").each do |rule_node|
        #     build_rule_and_add_to_matcher(rule_node)
        #   end
        # end
        # @visitor.on("Grammar_unnamed") do |grammar|
        #   grammar.captures("Rule").each do |rule_node|
        #     build_rule_and_add_to_matcher(rule_node)
        #   end
        # end
        
        @visitor.on("Grammar_named") do |grammar|
          grammar.captures("Rule").each do |rule_node|
            rule_name = rule_node.capture("ident").text
            rule_expr = rule_node.capture("RuleBody").visit(@visitor)
            @matcher.add_rule(rule_name, rule_expr)
          end
          @matcher
        end
        @visitor.on("Grammar_unnamed") do |grammar|
          grammar.captures("Rule").each do |rule_node|
            rule_name = rule_node.capture("ident").text
            rule_expr = rule_node.capture("RuleBody").visit(@visitor)
            @matcher.add_rule(rule_name, rule_expr)
          end
          @matcher
        end

        @visitor.on("RuleBody") do |rule_body|
          choice(rule_body.captures("TopLevelTerm").map(&.visit(@visitor)))
        end

        @visitor.on("TopLevelTerm_inline") do |top_level_term|
          label_name = top_level_term.capture("caseName").text
          top_level_term.capture("Seq").visit(@visitor).label(label_name)
        end
        @visitor.on("TopLevelTerm_seq") do |top_level_term|
          top_level_term.capture("Seq").visit(@visitor)
        end

        @visitor.on("Alt") do |alt_node|
          choice(alt_node.captures("Seq").map(&.visit(@visitor)))
        end

        @visitor.on("Seq") do |seq_node|
          seq(seq_node.captures("Pred").map(&.visit(@visitor)))
        end

        @visitor.on("Pred_neg") do |pred|
          neg(pred.capture("Iter").visit(@visitor))
        end
        @visitor.on("Pred_pos") do |pred|
          pos(pred.capture("Iter").visit(@visitor))
        end
        @visitor.on("Pred_iter") do |pred|
          pred.capture("Iter").visit(@visitor)
        end

        @visitor.on("Iter_star") do |iter|
          star(iter.capture("Label").visit(@visitor))
        end
        @visitor.on("Iter_plus") do |iter|
          plus(iter.capture("Label").visit(@visitor))
        end
        @visitor.on("Iter_opt") do |iter|
          opt(iter.capture("Label").visit(@visitor))
        end
        @visitor.on("Iter_label") do |iter|
          iter.capture("Label").visit(@visitor)
        end

        @visitor.on("Label_label") do |label|
          label_string = label.capture("ident").text
          label.capture("Base").visit(@visitor).label(label_string)
        end
        @visitor.on("Label_base") do |label|
          label.capture("Base").visit(@visitor)
        end

        @visitor.on("Base_application") do |base|
          rule_name = base.capture("ident").text
          apply(rule_name)
        end
        @visitor.on("Base_range") do |base|
          start_char_str : String = base.capture("start_char").capture("terminalChar").visit(@visitor)
          end_char_str : String = base.capture("end_char").capture("terminalChar").visit(@visitor)
          start_char = start_char_str[0]
          end_char = end_char_str[0]
          range(start_char..end_char)
        end
        @visitor.on("Base_terminal") do |base|
          base.capture("terminal").visit(@visitor)
        end
        @visitor.on("Base_group") do |base|
          base.capture("Alt").visit(@visitor)
        end
        @visitor.on("Base_dot") do |base|
          dot
        end

        @visitor.on("terminal") do |terminal|
          terminal_char_strs : Array(String) = terminal.captures("terminalChar").map(&.visit(@visitor))
          str = terminal_char_strs.join("")
          term(str)
        end

        @visitor.on("terminalChar_escape") do |terminal_char|
          terminal_char.capture("escapeChar").visit(@visitor)
        end
        @visitor.on("terminalChar_char") do |terminal_char|
          terminal_char.capture("char").text
        end

        @visitor.on("escapeChar_backslash") do |escape_char|
          "\\"
        end
        @visitor.on("escapeChar_doubleQuote") do |escape_char|
          "\""
        end
        @visitor.on("escapeChar_singleQuote") do |escape_char|
          "'"
        end
        @visitor.on("escapeChar_backspace") do |escape_char|
          "\b"
        end
        @visitor.on("escapeChar_lineFeed") do |escape_char|
          "\n"
        end
        @visitor.on("escapeChar_carriageReturn") do |escape_char|
          "\r"
        end
        @visitor.on("escapeChar_tab") do |escape_char|
          "\t"
        end
        @visitor.on("escapeChar_unicodeEscape") do |escape_char|
          hex_digits = escape_char.captures("hexDigit").map(&.text).join("")
          unicode_escape_sequence(hex_digits).to_s
        end
        @visitor.on("escapeChar_hexEscape") do |escape_char|
          hex_digits = escape_char.captures("hexDigit").map(&.text).join("")
          unicode_escape_sequence(hex_digits).to_s
        end

      end

      # Converts a Unicode hexadecimal string representation - either 2 hex chars, or 4 hex chars - into
      # the Unicode character that it represents, and returns the Char representation of that Unicode character.
      # Example:
      # > unicode_escape_sequence("A3")
      # => "£"
      def unicode_escape_sequence(escape_sequence : String) : Char
        escape_sequence.to_i(16).chr
      end

      # def build_rule_and_add_to_matcher(rule_node)
      #   rule_name = rule_node.capture("ident").text
      #   rule_expr = build_parse_expression_for_rule_body(rule_node.capture("RuleBody"))
      #   @matcher.add_rule(rule_name, rule_expr)
      # end

      # def build_parse_expression_for_rule_body(rule_body_node) : Expr
      #   top_level_terms = rule_body_node.captures("TopLevelTerm")
      #   choice()
      # end

      # takes a parse tree for an Arborist grammar definition and returns a Matcher that can build parse
      # trees for that grammar definition
      def build_grammar_parser(parse_tree : ApplyTree) : Matcher
        visitor = build_visitor
        matcher = visitor.visit(parse_tree)
        raise "something is wrong" unless matcher == @matcher
        @matcher
      end
    end
  end
end
