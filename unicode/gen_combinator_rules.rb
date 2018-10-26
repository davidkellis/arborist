#!/usr/bin/env ruby

# The following helper program generates the unicode_letter and unicode_digit rules.

# Instructions:
# 1. download https://www.unicode.org/Public/UCD/latest/ucdxml/ucd.all.flat.zip and unzip it, to produce ucd.all.flat.xml
# 2. cd to the directory where you put the ucd.all.flat.xml file, and then create a file called `print.rb`
# 3. paste the ruby program noted below into `print.rb`, and save the file
# 4. run the script: `ruby print.rb`
# 5. paste the printed output into your grammar file as the unicode_letter and unicode_digit rules
require 'nokogiri'

def unicode_min_ranges_for_general_categories(xml_doc, general_category_abbreviations = ["Lu", "Ll", "Lt", "Lm", "Lo"])
  node_set = general_category_abbreviations.map {|abbreviation| xml_doc.css("char[gc=#{abbreviation}]") }.reduce(:|)
  hex_encoded_code_points = node_set.map {|element| element.attr("cp") }
  int_encoded_code_points = hex_encoded_code_points.map(&:hex)
  int_encoded_code_points.sort!
  ranges = []
  contiguous_run = []
  prev = int_encoded_code_points.first - 1
  int_encoded_code_points.each do |i|
    if i != prev + 1
      ranges << [contiguous_run.first, contiguous_run.last]
      contiguous_run.clear
    end
    contiguous_run << i
    prev = i
  end
  ranges << [contiguous_run.first, contiguous_run.last]
  ranges
end

def convert_min_ranges_to_rules(ranges)
  char_ranges = ranges.map do |range|
    code_point1 = sprintf("%X", range.first)
    code_point2 = sprintf("%X", range.last)
    "range(Unicode.char(\"#{code_point1}\")..Unicode.char(\"#{code_point2}\"))"
  end.join(", ")
  "choice([#{char_ranges}] of Expr)"
end

def unicode_ranges(character_categories)
  xml_file_path = File.join(File.expand_path(File.dirname(__FILE__)), "ucd.all.flat.xml")
  doc = File.open(xml_file_path) { |f| Nokogiri::XML(f) }
  ranges = unicode_min_ranges_for_general_categories(doc, character_categories)
  convert_min_ranges_to_rules(ranges)
end

# Character class documentation is at http://www.unicode.org/reports/tr44/#General_Category_Values
puts "UnicodeDigit = #{unicode_ranges(["Nd"])}"
puts "UnicodeUpper = #{unicode_ranges(["Lu"])}"
puts "UnicodeLower = #{unicode_ranges(["Ll"])}"
puts "UnicodeTitlecase = #{unicode_ranges(["Lt"])}"
puts "UnicodeModifier = #{unicode_ranges(["Lm"])}"
puts "UnicodeOtherLetter = #{unicode_ranges(["Lo"])}"
puts "UnicodeLetter = choice(apply(\"unicode_upper\"), apply(\"unicode_lower\"), apply(\"unicode_titlecase\"), apply(\"unicode_modifier\"), apply(\"unicode_other_letter\"))"
