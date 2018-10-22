require "option_parser"

require "./arborist"
require "./grammar"

def run(grammar_file_path, input_file_path)
  grammar = Grammar.new(grammar_file)
  parse_tree = grammar.parse_file(input_file)
  if parse_tree
    puts parse_tree.to_msgpack
  else
    STDERR.puts "Failed parsing."
    exit(1)
  end
end

def main
  grammar_file = nil
  args = [] of String

  OptionParser.parse! do |parser|
    parser.banner = "Usage: arborist -e grammar_file.g file_to_parse.ext"
    parser.on("-g grammar_file.g", "Specifies the grammar file") { |file_name| grammar_file = file_name }
    parser.on("-h", "--help", "Show this help") { puts parser; exit }
    parser.invalid_option do |flag|
      STDERR.puts "ERROR: #{flag} is not a valid option."
      STDERR.puts parser
      exit(1)
    end
    parser.unknown_args do |unknown_args|
      args = unknown_args
    end
  end

  input_file = args.first?

  (STDERR.puts("A grammar file must be specified.") ; exit(1)) unless grammar_file
  (STDERR.puts("Grammar file \"#{grammar_file}\" does not exist") ; exit(1)) unless File.exists?(grammar_file.as(String))
  (STDERR.puts("Input file must be specified") ; exit(1)) unless input_file
  (STDERR.puts("Input file \"#{input_file}\" does not exist") ; exit(1)) unless File.exists?(input_file.as(String))

  run(grammar_file, input_file)
end

main