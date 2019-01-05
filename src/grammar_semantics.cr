require "./arborist"
require "./dsl"

module Arborist
  class Grammar
    class GrammarParserBuilder
      include Arborist::DSL

      @rule_name_to_parse_tree_map : Hash(String, ParseTree) = {} of String => ParseTree
      @parse_tree_to_mutex_alt_map : Hash(ParseTree, MutexAlt?) = {} of ParseTree => MutexAlt?

      # takes a parse tree for an Arborist grammar definition and returns a Matcher that can build parse
      # trees for that grammar definition
      def build_grammar_parser(grammar_parse_tree : ApplyTree) : Matcher
        @rule_name_to_parse_tree_map = build_rule_name_to_parse_tree_map(grammar_parse_tree)
        @parse_tree_to_mutex_alt_map = Hash(ParseTree, MutexAlt?).new
        # @parse_tree_to_mutex_alt_map = build_parse_tree_to_mutex_alt_map(grammar_parse_tree)
        build_parser_for_grammar(grammar_parse_tree)
      end

      # takes a grammar parse tree and returns a Hash(String, ParseTree) representing the rules as a map
      # of rule-name -> rule-body-parse-tree pairs
      def build_rule_name_to_parse_tree_map(parse_tree) : Hash(String, ParseTree)
        rule_name_to_parse_tree_map = Hash(String, ParseTree).new

        visitor = Visitor(Hash(String, ParseTree)).new
        visitor.on("Grammar_named") do |grammar|
          grammar.captures("Rule").each do |rule_node|
            rule_name = rule_node.capture("ident").text
            rule_body_parse_tree = rule_node.capture("RuleBody")    # this is an ApplyTree node; rule_body_parse_tree.child represents the ChoiceTree node that represents the RuleBody rule's parsed expression
            rule_name_to_parse_tree_map[rule_name] = rule_body_parse_tree.child
          end
          rule_name_to_parse_tree_map
        end
        visitor.on("Grammar_unnamed") do |grammar|
          grammar.captures("Rule").each do |rule_node|
            rule_name = rule_node.capture("ident").text
            rule_body_parse_tree = rule_node.capture("RuleBody")    # this is an ApplyTree node; rule_body_parse_tree.child represents the ChoiceTree node that represents the RuleBody rule's parsed expression
            rule_name_to_parse_tree_map[rule_name] = rule_body_parse_tree.child
          end
          rule_name_to_parse_tree_map
        end

        visitor.visit(parse_tree)   # return the rule-name -> rule-body-parse-tree pairs
      end

      # def build_parse_tree_to_mutex_alt_map(parse_tree : ParseTree) : Hash(ParseTree, MutexAlt?)
      #   @parse_tree_to_mutex_alt_map = Hash(ParseTree, MutexAlt?).new
      #   parse_tree.postorder_traverse(->(pt : ParseTree) {
      #     if pt.is_a?(ApplyTree)
      #       parse_tree_for_rule = pt.child
      #       @parse_tree_to_mutex_alt_map[pt] = build_mutex_alt_for_rule_parse_tree(pt.rule_name, parse_tree_for_rule, @rule_name_to_parse_tree_map)
      #     end
      #   })
      #   @parse_tree_to_mutex_alt_map
      # end

      # # returns a MutexAlt if the rule can be represented as a MutexAlt; nil otherwise
      # def build_mutex_alt_for_rule_name(rule_name : String, rule_name_to_parse_tree_map : Hash(String, ParseTree)) : MutexAlt?
      #   # to identify parse tree nodes that can represent a MutexAlt:
      #   # 1. find the transitive closure of every node in the parse tree
      #   # 2. for each node, ensure that all the nodes in that node's transitive closure are reducible to a MutexAlt
      #   # 3. for each node from (2) that is reducible to a MutexAlt, build a MutexAlt for that node and store 
      #   #    it in a Parse Tree Node -> MutexAlt map

      #   # to do this:
      #   # walk the rule parse trees, following rule applications, building up a set of string literals
      #   # if at any point you encounter a rule that would imply it can't be represented as a mutexalt, then raise
      #   parse_tree = rule_name_to_parse_tree_map[rule_name]
      #   strings = build_mutex_alt_string_set_for_rule_parse_tree(rule_name, parse_tree, rule_name_to_parse_tree_map, Set(ParseTree).new, Set(String).new)
      #   MutexAlt.new(strings) if is_mutex_alt_string_set_valid?(strings)
      # rescue MutexAltBuildFailure
      #   nil
      # end

      # `rule_name` is the name of the rule that, as a result of being evaluated, yielded the `parse_tree` node.
      # In other words, `rule_name` was the name of the rule that was actively being matched and produced a parse tree that
      # included the parse tree node represented by `parse_tree`.
      # `parse_tree` is the first (and only) child of an ApplyTree node.
      # Returns a MutexAlt if the parse tree can be represented as a MutexAlt; nil otherwise
      def build_mutex_alt_for_rule_parse_tree(rule_name : String, parse_tree : ParseTree, rule_name_to_parse_tree_map : Hash(String, ParseTree)) : MutexAlt?
        strings = build_mutex_alt_string_set_for_rule_parse_tree(rule_name, parse_tree, rule_name_to_parse_tree_map, Set(ParseTree).new, Set(String).new)
        MutexAlt.new(strings) if is_mutex_alt_string_set_valid?(strings)
      rescue MutexAltBuildFailure
        nil
      end

      def is_mutex_alt_string_set_valid?(strings : Set(String)) : Bool
        is_valid = !strings.empty?
        if is_valid
          string_length = strings.first.size
          is_valid &&= strings.all? {|str| str.size == string_length }
        end
        is_valid
      end

      class MutexAltBuildFailure < Exception; end
      def add_to_mutex_alt_set(strings : Set(String), new_string : String) : Set(String)
        # if !strings.empty?
        #   existing_string = strings.first
        #   raise MutexAltBuildFailure.new("Alternatives not all same string length.") unless existing_string.size == new_string.size
        # end
        strings << new_string
        strings
      end
      def add_to_mutex_alt_set(strings : Set(String), new_strings : Set(String)) : Set(String)
        # new_strings.each {|new_string| add_to_mutex_alt_set(strings, new_string) }
        strings.concat(new_strings)
        strings
      end


      def build_mutex_alt_string_set_for_rule_parse_tree(apply_tree : ApplyTree, 
                                              rule_name_to_parse_tree_map : Hash(String, ParseTree), 
                                              visited_nodes : Set(ParseTree),
                                              strings : Set(String)
                                              ) : Set(String)
        build_mutex_alt_string_set_for_rule_parse_tree(apply_tree.rule_name, apply_tree.child, rule_name_to_parse_tree_map, visited_nodes, strings)
      end

      # `parse_tree` is the first (and only) child of an ApplyTree node - it is the root tree node of a rule definition
      def build_mutex_alt_string_set_for_rule_parse_tree(rule_name : String,
                                              parse_tree : ParseTree, 
                                              rule_name_to_parse_tree_map : Hash(String, ParseTree), 
                                              visited_nodes : Set(ParseTree),
                                              strings : Set(String)
                                              ) : Set(String)
        return strings if visited_nodes.includes?(parse_tree)

        # if we already have a memoized string set captured in a memoized MutexAlt for this parse_tree, then, we just want
        # to add all the members of the memoized string set to `strings` and then return strings
        if (mutex_alt = @parse_tree_to_mutex_alt_map[parse_tree]?)
          strings.concat(mutex_alt.strings)
          return strings
        end

        visited_nodes << parse_tree

        case rule_name
        when "Grammar"
          raise MutexAltBuildFailure.new("Grammar rule encountered")
        when "Rule"
          raise MutexAltBuildFailure.new("Rule rule encountered")
        when "RuleBody"
          top_level_terms = parse_tree.captures("TopLevelTerm")
          if top_level_terms.size == 1
            build_mutex_alt_string_set_for_rule_parse_tree(top_level_terms.first.as(ApplyTree), rule_name_to_parse_tree_map, visited_nodes, strings)
          elsif top_level_terms.size > 1
            raise MutexAltBuildFailure.new("RuleBody rule has too many alternatives; it can only have one.")
          end
          # top_level_terms.each do |top_level_term|
          #   build_mutex_alt_string_set_for_rule_parse_tree(top_level_term.as(ApplyTree), rule_name_to_parse_tree_map, visited_nodes, strings)
          # end
        when "TopLevelTerm"
          seq = case top_level_alternative_label(parse_tree)
          when "inline"
            parse_tree.capture("Seq")
          when "seq"
            parse_tree.capture("seq")
          end
          build_mutex_alt_string_set_for_rule_parse_tree(seq.as(ApplyTree), rule_name_to_parse_tree_map, visited_nodes, strings)
        when "Alt"
          seq_nodes = parse_tree.captures("Seq")
          if seq_nodes.size == 1
            build_mutex_alt_string_set_for_rule_parse_tree(seq_nodes.first.as(ApplyTree), rule_name_to_parse_tree_map, visited_nodes, strings)
          elsif seq_nodes.size > 1
            raise MutexAltBuildFailure.new("Alt rule has too many alternatives; it can only have one.")
          end
          # seq_nodes.each do |seq|
          #   build_mutex_alt_string_set_for_rule_parse_tree(seq.as(ApplyTree), rule_name_to_parse_tree_map, visited_nodes, strings)
          # end
        when "Seq"
          pred_nodes = parse_tree.captures("Pred")
          if pred_nodes.size == 1
            build_mutex_alt_string_set_for_rule_parse_tree(pred_nodes.first.as(ApplyTree), rule_name_to_parse_tree_map, visited_nodes, strings)
          elsif pred_nodes.size > 1
            raise MutexAltBuildFailure.new("Seq rule has many terms")
          #elsif pred_nodes.size == 0
            # this is ok
          end
        when "Pred"
          case top_level_alternative_label(parse_tree)
          when "neg"
            raise MutexAltBuildFailure.new("Negative lookahead rule encountered")
          when "pos"
            raise MutexAltBuildFailure.new("Positive lookahead rule encountered")
          when "iter"
            iter = parse_tree.capture("iter")
            build_mutex_alt_string_set_for_rule_parse_tree(iter.as(ApplyTree), rule_name_to_parse_tree_map, visited_nodes, strings)
          end
        when "Iter"
          case top_level_alternative_label(parse_tree)
          when "star"
            raise MutexAltBuildFailure.new("0+ repetition rule encountered")
          when "plus"
            raise MutexAltBuildFailure.new("1+ repetition rule encountered")
          when "opt"
            raise MutexAltBuildFailure.new("Optional rule encountered")
          when "label"
            label = parse_tree.capture("label")
            build_mutex_alt_string_set_for_rule_parse_tree(label.as(ApplyTree), rule_name_to_parse_tree_map, visited_nodes, strings)
          end
        when "Label"
          case top_level_alternative_label(parse_tree)
          when "label"
            raise MutexAltBuildFailure.new("Labelled term encountered")
          when "base"
            base = parse_tree.capture("base")
            build_mutex_alt_string_set_for_rule_parse_tree(base.as(ApplyTree), rule_name_to_parse_tree_map, visited_nodes, strings)
          end
        # when "MutexAlt"
        #   case top_level_alternative_label(parse_tree)
        #   when "alts"
        #     base_trees = parse_tree.captures("Base")
        #     base_trees.each do |base|
        #       build_mutex_alt_string_set_for_rule_parse_tree(base.as(ApplyTree), rule_name_to_parse_tree_map, visited_nodes, strings)
        #     end
        #   when "base"
        #     base = parse_tree.capture("Base")
        #     build_mutex_alt_string_set_for_rule_parse_tree(base.as(ApplyTree), rule_name_to_parse_tree_map, visited_nodes, strings)
        #   end
        when "Base"
          case top_level_alternative_label(parse_tree)
          when "mutexAlt"
            mutexAlt = parse_tree.capture("mutexAlt")
            build_mutex_alt_string_set_for_rule_parse_tree(mutexAlt.as(ApplyTree), rule_name_to_parse_tree_map, visited_nodes, strings)
          when "application"
            referenced_rule_name = parse_tree.capture("ident").text
            parse_tree = rule_name_to_parse_tree_map[referenced_rule_name]
            rule_name = parse_tree.enclosing_rule_name || raise "Unable to identify the name of the grammar rule in the Arborist grammar that recognized/produced the parse tree for the rule body/definition of the #{referenced_rule_name} rule in the user's grammar."
            build_mutex_alt_string_set_for_rule_parse_tree(rule_name, parse_tree, rule_name_to_parse_tree_map, visited_nodes, strings)
          when "range"
            range = parse_tree.capture("range")
            build_mutex_alt_string_set_for_rule_parse_tree(range.as(ApplyTree), rule_name_to_parse_tree_map, visited_nodes, strings)
          when "terminal"
            terminal = parse_tree.capture("terminal")
            build_mutex_alt_string_set_for_rule_parse_tree(terminal.as(ApplyTree), rule_name_to_parse_tree_map, visited_nodes, strings)
          when "group"
            alt = parse_tree.capture("Alt")
            build_mutex_alt_string_set_for_rule_parse_tree(alt.as(ApplyTree), rule_name_to_parse_tree_map, visited_nodes, strings)
          when "dot"
            add_to_mutex_alt_set(strings, ALPHABET)
          end
        when "MutexAlt"
          mutex_alt_parse_trees = parse_tree.captures("mutexAltTerm")
          mutex_alt_parse_trees.each do |mutex_alt_parse_tree|
            build_mutex_alt_string_set_for_rule_parse_tree(mutex_alt_parse_tree.as(ApplyTree), rule_name_to_parse_tree_map, visited_nodes, strings)
          end
        when "mutexAltTerm"
          case top_level_alternative_label(parse_tree)
          when "application"
            referenced_rule_name = parse_tree.capture("ident").text
            parse_tree = rule_name_to_parse_tree_map[referenced_rule_name]
            rule_name = parse_tree.enclosing_rule_name || raise "Unable to identify the name of the grammar rule in the Arborist grammar that recognized/produced the parse tree for the rule body/definition of the #{referenced_rule_name} rule in the user's grammar."
            build_mutex_alt_string_set_for_rule_parse_tree(rule_name, parse_tree, rule_name_to_parse_tree_map, visited_nodes, strings)
          when "range"
            range = parse_tree.capture("range")
            build_mutex_alt_string_set_for_rule_parse_tree(range.as(ApplyTree), rule_name_to_parse_tree_map, visited_nodes, strings)
          when "terminal"
            terminal = parse_tree.capture("terminal")
            build_mutex_alt_string_set_for_rule_parse_tree(terminal.as(ApplyTree), rule_name_to_parse_tree_map, visited_nodes, strings)
          when "dot"
            add_to_mutex_alt_set(strings, ALPHABET)
          end
        when "range"
          string_set = build_string_set_from_range_rule_application_parse_tree(parse_tree)
          add_to_mutex_alt_set(strings, string_set)
        when "terminal"
          string_set = build_string_set_from_terminal(parse_tree)
          add_to_mutex_alt_set(strings, string_set)
        when "caseName"
        when "name"
        when "nameFirst"
        when "nameRest"
        when "ident"
        when "oneCharTerminal"
        when "terminalChar"
        when "escapeChar"
        when "skip"
        when "space"
        when "comment"
        when "alnum"
        when "letter"
        when "digit"
        when "hexDigit"
        when "unicode_digit"
        when "unicode_upper"
        when "unicode_lower"
        when "unicode_titlecase"
        when "unicode_modifier"
        when "unicode_other_letter"
        when "unicode_letter"
        else
          raise "Unknown parse tree rule: #{rule_name}"
        end

        strings
      end

      def top_level_alternative_label(parse_tree : ParseTree) : String
        parse_tree = parse_tree.child if parse_tree.is_a?(ApplyTree)
        if child_tree_label = parse_tree.child.label()
          child_tree_label
        else
          raise "ParseTree does not have a top-level named alternative."
        end
      end

      # returns a Matcher that knows how to parse
      # parse_tree is a ParseTree produced by Arborist::Grammar::Rules::GrammarParser
      def build_parser_for_grammar(parse_tree) : Matcher
        matcher = Matcher.new

        visitor = Visitor(Matcher | Expr | String).new
        visitor.on("Grammar_named") do |grammar|
          grammar.captures("Rule").each do |rule_node|
            rule_name = rule_node.capture("ident").text
            rule_expr = rule_node.capture("RuleBody").visit(visitor).as(Expr)
            matcher.add_rule(rule_name, rule_expr)
          end
          matcher
        end
        visitor.on("Grammar_unnamed") do |grammar|
          grammar.captures("Rule").each do |rule_node|
            rule_name = rule_node.capture("ident").text
            rule_expr = rule_node.capture("RuleBody").visit(visitor).as(Expr)
            matcher.add_rule(rule_name, rule_expr)
          end
          matcher
        end

        visitor.on("RuleBody") do |parse_tree|
          choice(parse_tree.captures("TopLevelTerm").map(&.visit(visitor).as(Expr)))
        end

        visitor.on("TopLevelTerm_inline") do |parse_tree|
          label_name = parse_tree.capture("caseName").capture("name").text
          parse_tree.capture("Seq").visit(visitor).as(Expr).label(label_name)
        end
        visitor.on("TopLevelTerm_seq") do |parse_tree|
          parse_tree.capture("seq").visit(visitor)
        end

        visitor.on("Alt") do |parse_tree|
          choice(parse_tree.captures("Seq").map(&.visit(visitor).as(Expr)))
        end

        visitor.on("Seq") do |parse_tree|
          seq(parse_tree.captures("Pred").map(&.visit(visitor).as(Expr)))
        end

        visitor.on("Pred_neg") do |parse_tree|
          neg(parse_tree.capture("Iter").visit(visitor).as(Expr))
        end
        visitor.on("Pred_pos") do |parse_tree|
          pos(parse_tree.capture("Iter").visit(visitor).as(Expr))
        end
        visitor.on("Pred_iter") do |parse_tree|
          parse_tree.capture("iter").visit(visitor)
        end

        visitor.on("Iter_star") do |parse_tree|
          star(parse_tree.capture("Label").visit(visitor).as(Expr))
        end
        visitor.on("Iter_plus") do |parse_tree|
          plus(parse_tree.capture("Label").visit(visitor).as(Expr))
        end
        visitor.on("Iter_opt") do |parse_tree|
          opt(parse_tree.capture("Label").visit(visitor).as(Expr))
        end
        visitor.on("Iter_label") do |parse_tree|
          parse_tree.capture("label").visit(visitor)
        end

        visitor.on("Label_label") do |parse_tree|
          label_string = parse_tree.capture("ident").text
          parse_tree.capture("Base").visit(visitor).as(Expr).label(label_string)
        end
        visitor.on("Label_base") do |parse_tree|
          parse_tree.capture("base").visit(visitor)
        end

        # If a MutexAlt application represents a set of terminals of the same length (implied by the 
        # presence of "|" operators, and therefore alternatives) ...
        # (case 1) ... then we want to return a MutexAlt,
        # visitor.on("MutexAlt_alts") do |parse_tree|
        #   base_apply_trees = parse_tree.captures("Base").map(&.as(ApplyTree))
        #   string_alts = get_mutex_alt_arguments_from_rule_applications(visitor, base_apply_trees)
        #   alt(string_alts)
        # end
        # # (case 2) ...otherwise we want to fall through to the wrapped rules
        # visitor.on("MutexAlt_base") do |parse_tree|
        #   parse_tree.capture("base").visit(visitor)
        # end

        visitor.on("Base_mutexAlt") do |parse_tree|
          parse_tree.capture("mutexAlt").visit(visitor)
        end
        visitor.on("Base_application") do |parse_tree|
          rule_name = parse_tree.capture("ident").text
          apply(rule_name)
        end
        visitor.on("Base_range") do |parse_tree|
          parse_tree.capture("range").visit(visitor)
        end
        visitor.on("Base_terminal") do |parse_tree|
          parse_tree.capture("terminal").visit(visitor)
        end
        visitor.on("Base_group") do |parse_tree|
          parse_tree.capture("Alt").visit(visitor)
        end
        visitor.on("Base_dot") do |parse_tree|
          dot
        end

        # A MutexAlt application represents a set of terminals of the same length (implied by the 
        # presence of "|" operators, and therefore alternatives), so we want to build up a MutexAlt
        # and then return it
        visitor.on("MutexAlt") do |parse_tree|
          mutex_alt_term_parse_trees = parse_tree.captures("mutexAltTerm").map(&.as(ApplyTree))
          mutex_alts = mutex_alt_term_parse_trees.map do |apply_tree|
            rule_name = apply_tree.rule_name
            rule_parse_tree_body = apply_tree.child
            mutex_alt = build_mutex_alt_for_rule_parse_tree(rule_name, rule_parse_tree_body, @rule_name_to_parse_tree_map)
            mutex_alt || raise "Unable to build MutexAlt for parse tree. It must be an invalid MutexAlt specification."
          end
          alt(mutex_alts)
        end
        visitor.on("mutexAltTerm_application") do |parse_tree|
          raise "Error: mutexAltTerm_application should not be visited."
        end
        visitor.on("mutexAltTerm_range") do |parse_tree|
          raise "Error: mutexAltTerm_range should not be visited."
        end
        visitor.on("mutexAltTerm_terminal") do |parse_tree|
          raise "Error: mutexAltTerm_terminal should not be visited."
        end
        visitor.on("mutexAltTerm_dot") do |parse_tree|
          raise "Error: mutexAltTerm_dot should not be visited."
        end

        visitor.on("range") do |parse_tree|
          # start_char_str : String = range.capture("start_char").capture("terminalChar").visit(visitor).as(String)
          # end_char_str : String = range.capture("end_char").capture("terminalChar").visit(visitor).as(String)
          # start_char = start_char_str[0]
          # end_char = end_char_str[0]
          # range(start_char..end_char)
          # build_mutex_alt_expr_from_range_rule_application_parse_tree(parse_tree)
          string_set = build_string_set_from_range_rule_application_parse_tree(parse_tree)
          alt(string_set)
        end

        visitor.on("terminal") do |parse_tree|
          terminal_char_strs : Array(String) = parse_tree.captures("terminalChar").map(&.visit(visitor).as(String))
          str = terminal_char_strs.join("")
          term(str)
        end

        visitor.on("terminalChar_escape") do |terminal_char|
          terminal_char.capture("escape").visit(visitor)
        end
        visitor.on("terminalChar_char") do |terminal_char|
          terminal_char.capture("char").text
        end

        visitor.on("escapeChar_backslash") do |escape_char|
          "\\"
        end
        visitor.on("escapeChar_doubleQuote") do |escape_char|
          "\""
        end
        visitor.on("escapeChar_singleQuote") do |escape_char|
          "'"
        end
        visitor.on("escapeChar_backspace") do |escape_char|
          "\b"
        end
        visitor.on("escapeChar_lineFeed") do |escape_char|
          "\n"
        end
        visitor.on("escapeChar_carriageReturn") do |escape_char|
          "\r"
        end
        visitor.on("escapeChar_tab") do |escape_char|
          "\t"
        end
        visitor.on("escapeChar_unicodeEscapeLong") do |escape_char|
          hex_digits = escape_char.captures("hexDigit").map(&.text).join("")
          Unicode.char(hex_digits).to_s
        end
        visitor.on("escapeChar_unicodeEscape") do |escape_char|
          hex_digits = escape_char.captures("hexDigit").map(&.text).join("")
          Unicode.char(hex_digits).to_s
        end
        visitor.on("escapeChar_hexEscape") do |escape_char|
          hex_digits = escape_char.captures("hexDigit").map(&.text).join("")
          Unicode.char(hex_digits).to_s
        end

        visitor.visit(parse_tree).as(Matcher)   # return the generated Matcher that can parse the grammar definition represented by the given parse tree
      end

      def build_string_set_from_range_rule_application_parse_tree(range_rule_body : ParseTree) : Set(String)
        start_char_str : String = terminal_char_to_string(range_rule_body.capture("start_char").capture("terminalChar"))
        end_char_str : String = terminal_char_to_string(range_rule_body.capture("end_char").capture("terminalChar"))
        start_char = start_char_str[0]
        end_char = end_char_str[0]
        retval = (start_char..end_char).map(&.to_s).to_set
        retval
      end

      def build_string_set_from_terminal(terminal_rule_body : ParseTree) : Set(String)
        str = terminal_rule_body.captures("terminalChar").map {|terminal_char| terminal_char_to_string(terminal_char) }.join("")
        Set{str}
      end

      def terminal_char_to_string(terminal_char : ParseTree) : String
        case top_level_alternative_label(terminal_char)
        when "escape"
          escape_char_to_string(terminal_char.capture("escape"))
        when "char"
          terminal_char.text
        else
          raise "Unknown terminalChar: #{terminal_char.text}"
        end
      end

      def escape_char_to_string(parse_tree : ParseTree) : String
        case top_level_alternative_label(parse_tree)
        when "backslash"
          "\\"
        when "doubleQuote"
          "\""
        when "singleQuote"
          "'"
        when "backspace"
          "\b"
        when "lineFeed"
          "\n"
        when "carriageReturn"
          "\r"
        when "tab"
          "\t"
        when "unicodeEscapeLong"
          hex_digits = parse_tree.captures("hexDigit").map(&.text).join("")
          Unicode.char(hex_digits).to_s
        when "unicodeEscape"
          hex_digits = parse_tree.captures("hexDigit").map(&.text).join("")
          Unicode.char(hex_digits).to_s
        when "hexEscape"
          hex_digits = parse_tree.captures("hexDigit").map(&.text).join("")
          Unicode.char(hex_digits).to_s
        else
          raise "Unknown escapeChar: #{parse_tree.text}"
        end
      end

      def mutex_alt?(parse_tree_node : ParseTree) : MutexAlt?
        @parse_tree_to_mutex_alt_map[parse_tree_node]?
      end
    end

  end
end

# visitor method skeleton
# def visitor_method(apply_tree : ApplyTree) : Hash(ParseTree, MutexAlt?)
#   case apply_tree.rule_name
#   when "Grammar"
#     case top_level_alternative_label(apply_tree)
#     when "named"
#     when "unnamed"
#     end
#   when "Rule"
#   when "RuleBody"
#   when "TopLevelTerm"
#     case top_level_alternative_label(apply_tree)
#     when "inline"
#     when "seq"
#     end
#   when "Alt"
#   when "Seq"
#   when "Pred"
#     case top_level_alternative_label(apply_tree)
#     when "neg"
#     when "pos"
#     when "iter"
#     end
#   when "Iter"
#     case top_level_alternative_label(apply_tree)
#     when "star"
#     when "plus"
#     when "opt"
#     when "label"
#     end
#   when "Label"
#     case top_level_alternative_label(apply_tree)
#     when "label"
#     when "mutex_alt"
#     end
#   when "MutexAlt"
#     case top_level_alternative_label(apply_tree)
#     when "alts"
#     when "base"
#     end
#   when "Base"
#     case top_level_alternative_label(apply_tree)
#     when "application"
#     when "range"
#     when "terminal"
#     when "group"
#     when "dot"
#     end
#   when "range"
#   when "caseName"
#   when "name"
#   when "nameFirst"
#   when "nameRest"
#   when "ident"
#   when "terminal"
#   when "oneCharTerminal"
#   when "terminalChar"
#     case top_level_alternative_label(apply_tree)
#     when "escape"
#     when "char"
#     end
#   when "escapeChar"
#     case top_level_alternative_label(apply_tree)
#     when "backslash"
#     when "doubleQuote"
#     when "singleQuote"
#     when "backspace"
#     when "lineFeed"
#     when "carriageReturn"
#     when "tab"
#     when "unicodeEscapeLong"
#     when "unicodeEscape"
#     when "hexEscape"
#     end
#   when "skip"
#   when "space"
#   when "comment"
#     case top_level_alternative_label(apply_tree)
#     when "singleLine"
#     when "multiLine"
#     end
#   when "alnum"
#   when "letter"
#   when "digit"
#   when "hexDigit"
#   when "unicode_digit"
#   when "unicode_upper"
#   when "unicode_lower"
#   when "unicode_titlecase"
#   when "unicode_modifier"
#   when "unicode_other_letter"
#   when "unicode_letter"
#   else
#     raise "Unknown parse tree rule: #{apply_tree.rule_name}"
#   end
# end