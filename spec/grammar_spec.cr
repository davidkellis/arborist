require "./spec_helper"

include Arborist
include Arborist::DSL

describe Arborist do
  describe Grammar do
    it "loads a grammar definition" do
      g = Grammar.new("spec/testgrammar1.arborist")

      g.rules.size.should eq(1)

      g.parse("").should be_nil
      g.parse("blah").should be_nil
      g.parse("foo").try(&.syntax_tree).should eq(["foo"])
    end

    it "loads a more complicated grammar definition" do
      g = Grammar.new("spec/testgrammar2.arborist")

      g.rules.size.should eq(7)
      g.rules.keys.sort.should eq(["Start", "bar", "baz", "cap", "foo", "qux", "skip"])

      g.parse("").should be_nil
      g.parse("blah").should be_nil
      g.parse("A").try(&.syntax_tree).should eq([["A"]])
      g.parse("foobarbaz").try(&.syntax_tree).should eq([["foo"], [["bar"]], [["baz"]]])
      g.parse("  foo  bar  baz").try(&.syntax_tree).should eq([["foo"], [["bar"]], [["baz"]]])
    end
  end

  describe Grammar::Unicode do
    it "Converts a Unicode hexadecimal string representation - either 2 hex chars, or 4 hex chars - into
        the Unicode character that it represents, and returns the Char representation of that Unicode character." do
      Grammar::Unicode.char("A3").should eq('£')
      Grammar::Unicode.char("41").should eq('A')
      Grammar::Unicode.char("5A").should eq('Z')
      Grammar::Unicode.char("5a").should eq('Z')
    end
  end
end
