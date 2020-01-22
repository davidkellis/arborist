require "./arborist"
require "./matcher"
require "./grammar_rules"
require "./grammar_semantics"

module Arborist
  class Grammar
    module Unicode
      # Converts a Unicode hexadecimal string representation - either 2 hex chars, or 4 hex chars - into
      # the Unicode character that it represents, and returns the Char representation of that Unicode character.
      # Example:
      # > Unicode.char("A3")
      # => "£"
      def self.char(hex_escape_sequence : String) : Char
        hex_escape_sequence.to_i(16).chr
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

    def build_matcher(grammar_defn : String) : Matcher?
      grammar_parse_tree = Rules::GrammarParser.match(grammar_defn, "Grammar")
      if grammar_parse_tree
        GrammarParserBuilder.new.build_grammar_parser(grammar_parse_tree)
      else
        Rules::GrammarParser.print_match_failure_error
        raise "Unable to parse grammar file."
      end
    end

    def parse_file(path : String, mode : Symbol = :ohm)
      file_contents = File.read(path)
      parse(file_contents, mode)
    end

    def parse(input_str : String, mode : Symbol = :ohm) : ParseTree?
      matcher = @matcher
      raise "No grammar definition has been loaded. Please load a grammar definition before trying to parse." unless matcher

      matcher.set_mode(mode)
      
      matcher.match(input_str)
    end

    def parse(input_str : String, start_rule : String) : ParseTree?
      matcher = @matcher
      raise "No grammar definition has been loaded. Please load a grammar definition before trying to parse." unless matcher
      
      matcher.match(input_str, start_rule)
    end

    def rules
      @matcher.try(&.rules) || Hash(String, Rule).new
    end

    def print_match_failure_error
      @matcher.try(&.print_match_failure_error)
    end
  end
end
