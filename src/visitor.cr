require "./parse_tree"

module Arborist
  class Visitor(R)
    @visitors : Hash(String, Proc(ParseTree, R))

    def initialize
      @visitors = {} of String => Proc(ParseTree, R)
    end

    def on(rule_name, &block : ParseTree -> R)
      @visitors[rule_name] = block
    end

    def visit(root_tree_node : ApplyTree) : R
      visitor = @visitors[root_tree_node] || raise "No visitor function defined for rule \"#{root_tree_node.rule_name}\""
      visitor.call(root_tree_node)
    end
  end
end