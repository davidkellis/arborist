require "option_parser"

require "./grammar"
require "./parse_tree"

def run(grammar_file_path, input_file_path, print_mode)
  # puts "loading grammar:"
  grammar = Arborist::Grammar.new(grammar_file_path)
  # puts "parsing input file:"
  parse_tree = grammar.parse_file(input_file_path)
  if parse_tree
    case print_mode
    when :sexp
      puts parse_tree.s_exp
    when :binary
      STDOUT.write(Arborist::ParseTree.to_msgpack(parse_tree))
    end
  else
    grammar.print_match_failure_error
    STDERR.puts "Failed parsing."
    exit(1)
  end
rescue e
  STDERR.puts "Failed to parse: #{e.message}"
  STDERR.puts e.backtrace.join("\n") if Config.debug
  exit(1)
end

class Config
  class_property debug : Bool = false
  class_property grammar_file : String?
  class_property args : Array(String) = [] of String
end

def main
  grammar_file = nil
  args = [] of String
  print_mode = :binary

  OptionParser.parse! do |parser|
    parser.banner = "Usage: arborist -e grammar_file.g file_to_parse.ext"
    parser.on("-d", "Enable debug mode.") { Config.debug = true }
    parser.on("-g grammar_file.g", "Specifies the grammar file") { |file_name| Config.grammar_file = file_name }
    parser.on("-h", "--help", "Show this help") { puts parser; exit }
    parser.on("-s", "Print the parse tree as an s-expression. (default is binary mode)") { print_mode = :sexp }
    parser.invalid_option do |flag|
      STDERR.puts "ERROR: #{flag} is not a valid option."
      STDERR.puts parser
      exit(1)
    end
    parser.unknown_args do |unknown_args|
      Config.args = unknown_args
    end
  end

  input_file = Config.args.first?

  (STDERR.puts("A grammar file must be specified.") ; exit(1)) unless Config.grammar_file
  (STDERR.puts("Grammar file \"#{grammar_file}\" does not exist") ; exit(1)) unless File.exists?(Config.grammar_file.as(String))
  (STDERR.puts("Input file must be specified") ; exit(1)) unless input_file
  (STDERR.puts("Input file \"#{input_file}\" does not exist") ; exit(1)) unless File.exists?(input_file.as(String))

  run(Config.grammar_file.as(String), input_file.as(String), print_mode)
end

main