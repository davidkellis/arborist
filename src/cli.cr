require "option_parser"

require "./grammar"
require "./parse_tree"

def run(grammar_file_path, input_file_path, print_mode, print_timings, parsing_mode)
  # puts "loading grammar:"
  t1 = Time.local
  grammar = Arborist::Grammar.new(grammar_file_path)
  t2 = Time.local
  # puts "parsing input file:"
  Arborist::GlobalDebug.enable! if Config.debug
  parse_tree = grammar.parse_file(input_file_path, parsing_mode)
  t3 = Time.local
  if print_timings
    puts "timings: load grammar = #{t2 - t1} ; parse = #{t3 - t2}"
  end
  if parse_tree
    case print_mode
    when :simple
      puts parse_tree.simple_s_exp
    when :sexp
      puts parse_tree.s_exp
    when :binary
      STDOUT.write(Arborist::ParseTree.to_msgpack(parse_tree))
    when :recognize_only
      puts "Input conforms to grammar."
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
  class_property parsing_mode : Symbol = :ohm
  class_property grammar_file : String?
  class_property args : Array(String) = [] of String
end

def main
  grammar_file = nil
  args = [] of String
  print_mode = :binary
  print_timings = false

  OptionParser.parse do |parser|
    parser.banner = "Usage: arborist -g grammar_file.g file_to_parse.ext"
    parser.on("-d", "Enable debug mode.") { Config.debug = true }
    parser.on("-m mode", "Specify parsing mode: ohm (default), python, simple.\n\t\t\t\t\tohm mode enables syntactic rule semantics (i.e. rules that start with a capital letter automatically skip whitespace between expression terms).\n\t\t\t\t\tpython mode enables indent/dedent rule semantics in which indent/dedent is treated as lexical delimiters\n\t\t\t\t\tsimple mode disables ohm mode and python mode\n\t\t\t\t\t(default is ohm mode)") {|mode|
      modes = {"ohm" => :ohm, "python" => :python, "simple" => :simple}
      if mode_symbol = modes[mode]?
        Config.parsing_mode = mode_symbol
      else
        STDERR.puts "ERROR: Mode #{mode} is not a valid mode specifier. The mode must be one of the following three options: ohm, python, simple"
        STDERR.puts parser
        exit(1)
      end
    }
    parser.on("-g grammar_file.g", "Specifies the grammar file") { |file_name| Config.grammar_file = file_name }
    parser.on("-h", "--help", "Show this help") { puts parser; exit }
    parser.on("--simple", "Print the parse tree as simple one-line s-expression. (default is binary mode)") { print_mode = :simple }
    parser.on("-s", "Print the parse tree as an s-expression. (default is binary mode)") { print_mode = :sexp }
    parser.on("-r", "Recognize only; do not print the parse tree. (default is binary mode)") { print_mode = :recognize_only }
    parser.on("-t", "Print time to load grammar and parse. (default to false)") { print_timings = true }
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

  run(Config.grammar_file.as(String), input_file.as(String), print_mode, print_timings, Config.parsing_mode)
end

main