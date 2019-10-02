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

    def visit(apply_node : ApplyTree) : R
      visitor_fn = @visitors[apply_node.rule_name]?
      return visitor_fn.call(apply_node) if visitor_fn

      # If the `apply_node` is a reference to a rule with multiple top-level alternatives that are all named, then try to look
      # up the visitor function for one of the top-level labeled alternatives
      # Keep in mind that a ChoiceTree will only represent one alternative taken from the top-level alternatives defined 
      # in the corresponding Choice object that created this ChoiceTree. The Choice object actually represents the different
      # top-level alternative expressions. The ChoiceTree only represents one of those realized top-level alternatives.
      choice_tree = apply_node.tree.as?(ChoiceTree)
      if choice_tree && choice_tree.tree.labeled?
        visitor_function_name = "#{apply_node.rule_name}_#{choice_tree.tree.label}"
        visitor_fn = @visitors[visitor_function_name]? || raise "No visitor function defined for rule \"#{visitor_function_name}\""
        return visitor_fn.call(apply_node)
      end

      raise "No visitor function defined for rule \"#{apply_node.rule_name}\". \nroot = \n#{apply_node.root.s_exp} \n\n\n\napply_node.s_exp = \n#{apply_node.s_exp}"
    end
  end
end