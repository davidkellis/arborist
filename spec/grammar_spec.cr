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
      g.parse("foo").try(&.syntax_tree).should eq("foo")
    end

    it "loads a more complicated grammar definition" do
      g = Grammar.new("spec/testgrammar2.arborist")

      g.rules.size.should eq(5)
      g.rules.keys.sort.should eq(["Start", "bar", "baz", "foo", "qux"])

      g.parse("").should be_nil
      g.parse("blah").should be_nil
      g.parse("foobarbaz").try(&.syntax_tree).should eq(["foo", ["bar"], ["baz"]])
      g.parse("  foo  bar  baz").try(&.syntax_tree).should eq(["foo", ["bar"], ["baz"]])
    end
  end
end
