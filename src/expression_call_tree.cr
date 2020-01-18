module Arborist
  class ExprCallTree
    property expr_call : ExprCall
    property parent : ExprCallTree?
    property children : Array(ExprCallTree)

    def initialize(@expr_call, @parent)
      @children = [] of ExprCallTree
    end

    # returns the newly created child tree node
    def append_child(expr_call : ExprCall)
      append_child(ExprCallTree.new(expr_call, self))
    end

    # returns the child tree node
    def append_child(child_tree : ExprCallTree)
      @children << child_tree
      child_tree
    end

    def self_and_descendants
      nodes = [] of ExprCallTree
      postorder_traverse do |expr_call_tree|
        nodes << expr_call_tree
      end
      nodes
    end

    def postorder_traverse(&blk : ExprCallTree -> )
      children.each {|child| child.postorder_traverse(&blk) }
      blk.call(self)
    end

    def hash(hasher)
      self.object_id.hash(hasher)
    end
  end

  class ExprCallTreeController
    property root : ExprCallTree?
    property current_node : ExprCallTree?

    def initialize()
      reset
    end

    def reset
      @root = nil
      @current_node = nil
    end

    def push_onto_call_stack(expr_call : ExprCall)
      if current_node = @current_node
        @current_node = current_node.append_child(expr_call)
      else
        @root = @current_node = ExprCallTree.new(expr_call, nil)
      end
    end

    def pop_off_of_call_stack()
      if current_node = @current_node
        @current_node = current_node.parent
        reset unless @current_node
      end
      @current_node
    end

    def current_expr_call_failed()
      # @current_node and all descendant nodes should be removed from the child_recursive_calls of any ancestor node
      #
      # I will implement this by having each ExprCall that is added as a child_recursive_call also track which parent
      # it was added as a child of, and then the ExprCall that was added as a child will know both the parent and child
      # nodes in which the child needs to be removed from the parent's child_recursive_call set.
      if current_node = @current_node
        expr_calls_that_descend_from_current_failed_expr_call_tree = current_node.self_and_descendants.map(&.expr_call)
        expr_calls_that_descend_from_current_failed_expr_call_tree.each do |expr_call|
          expr_call.remove_self_from_parent_recursive_call if expr_call.is_a?(ApplyCall)
        end
      end
    end

  end
end