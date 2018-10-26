module Arborist
  module DSL
    ALPHABET = ((' '..'~').map(&.to_s) + ["\n"] + ["\t"])

    def label(label : String, expr : Expr) : Expr
      expr.label(label)
      expr
    end

    def term(string : String) : Expr
      Terminal.new(string)
    end

    def choice(*alternatives) : Expr
      choice(alternatives.map(&.as(Expr)).to_a)
    end

    def choice(alternatives : Array(Expr)) : Expr
      Choice.new(alternatives)
    end

    def apply(rule_name : String) : Expr
      Apply.new(rule_name)
    end

    def range(chars : Range(Char, Char)) : Expr
      terms = chars.map {|char| term(char.to_s).as(Expr) }
      choice(terms)
    end

    def dot(alphabet = ALPHABET) : Expr
      terms = alphabet.map {|char| term(char.to_s).as(Expr) }
      choice(terms)
    end

    def seq(*exprs) : Expr
      seq(exprs.map(&.as(Expr)).to_a)
    end

    def seq(exprs : Array(Expr)) : Expr
      Sequence.new(exprs)
    end
    
    # this represents the optional operator `?` - 0 or 1 repetitions
    def opt(expr : Expr) : Expr
      Optional.new(expr)
    end

    # this represents the kleene star operator - 0+ repetitions
    def star(expr : Expr) : Expr
      Repetition.new(expr)
    end

    # this represents 1+ repetitions
    def plus(expr : Expr) : Expr
      # seq([expr, star(expr)] of Expr)
      RepetitionOnePlus.new(expr)
    end

    # not predicate - negative lookahead
    def neg(expr : Expr) : Expr
      NegLookAhead.new(expr)
    end
    
    # and predicate - positive lookahead
    def pos(expr : Expr) : Expr
      PosLookAhead.new(expr)
    end
  end
end