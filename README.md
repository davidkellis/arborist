# arborist

Arborist is a PEG parser that supports left and right recursion.

Arborist may be used as a PEG parser combinator library or as a command line tool that parses a given input file according to a grammar file and prints the resulting parse tree.

Features
- Left recursion
- Right recursion
- May be used as a PEG parser combinator library
- May be used as a command line parser that produces a parse tree for a given input document

When used as a Crystal library, Arborist can be used to parse input documents that conform to a grammar, and can also be used
to define and execute semantic actions for a grammar.

When used as a command line tool, Arborist can take a grammar file and parse an input document to produce a parse tree that may then
be written to a file or piped to another tool for further processing/manipulation. The parse tree processing tool may be written
in any lanugage so long as it can read the parse tree format (documentation for the parse tree format is coming soon) that Arborist writes out.


## Usage

### Command Line Interface

```bash
$ cat mygrammar.g
E <- E - E / E + E / Num
Num <- ("0".."9")+

$ cat input_document.txt
1-2+3-4+5

$ arborist -g mygrammar.g input_document.txt
[[[[["1"], "-", ["2"]], "+", ["3"]], "-", ["4"]], "+", ["5"]]
```

The parse tree written to stdout may be redirected to a file, piped to another tool, etc.

### Library

Add this to your application's `shard.yml`:

```yaml
dependencies:
  arborist:
    github: davidkellis/arborist
```

Then use it like this:

```crystal
require "arborist"

include Arborist::DSL

# build the parser

# e <- e1=e - e2=e -- subtract
#   / exprs+=e ("+" exprs+=e)* -- add
#   / num         -- num
# num <- [0-9]+
e = choice(
  seq(apply("e").label("e1"), term("-"), apply("e").label("e2")).label("subtract"), 
  seq(apply("e").label("exprs"), star(seq(term("+"), apply("e").label("exprs")))).label("add"), 
  apply("num").label("num")
)
num = plus(range('0'..'9'))
m1 = Matcher.new.add_rule("e", e).add_rule("num", num)

# parse an expression into a parse tree

parse_tree = m1.match("1-2+10-3+10", "e")

puts "expression: 1-2+10-3+10"
puts "parse tree:"
puts parse_tree.try(&.syntax_tree)

# create a visitor to traverse the parse tree

eval = Visitor(Int32).new

eval.on("e_subtract") do |ctx|
  # ctx.get("e1").visit - ctx.e2.visit
  # eval.visit(ctx.capture("e1")) - ctx.capture("e2").visit(eval)
  ctx.capture("e1").visit(eval) - ctx.capture("e2").visit(eval)
end
eval.on("e_add") do |ctx|
  # ctx.all("exprs").map(&.visit)
  ctx.captures("exprs").map(&.visit(eval)).sum
end
eval.on("e_num") do |ctx|
  ctx.capture("num").visit(eval)
end

eval.on("num") do |ctx|
  ctx.text.to_i
end

# evaluate the visitor against the parse tree
raise "invalid parse tree" unless parse_tree
puts "evaluated value:"
puts eval.visit(parse_tree)
```

prints

```
expression: 1-2+10-3+10
parse tree:
[[[[["1"], "-", ["2"]], [["+", ["1", "0"]]]], "-", ["3"]], [["+", ["1", "0"]]]]
evaluated value:
16
```


## Grammar Definition Syntax

### General

A grammar may consist of the following parsing expressions:
- Rule application
- Ordered choice
- Sequence
- Terminal
- Mutually Exclusive Alternation
- Positive look-ahead / And predicate
- Negative look-ahead / Not predicate
- Optional
- Zero-or-more repetition
- One-or-more repetition

Arborist supports [Ohm's](https://github.com/harc/ohm/blob/master/doc/syntax-reference.md#syntactic-vs-lexical-rules) 
distinction between syntactic and lexical rules.

Syntactic rules have rule names that start with an uppercase letter.
Lexical rules have rule names that start with a lowercase letter.

Syntactic rules implicitly consume and ignore whitespace that occurs immediately prior to any of the terms that make up the rule body. Syntactic rules do **not** consume whitespace occurring after the application of the syntactic rule.

Lexical rules do not implicitly consume whitespace. Lexical rules are just standard PEG parser rules. If you need to treat
whitespace as a significant feature in a grammar rule, then you should define the grammar rule as a lexical rule, and not
a syntactic rule.


### Grammar Sample

```
Arithmetic {
  Expr
    <- Add

  Add
    <- Add "+" Add  -- plus
    / Add "-" Add  -- minus
    / Mult

  Mult
    <- Mult "*" Mult  -- times
    / Mult "/" Mult  -- divide
    / Exp

  Exp
    <- Primary "^" Exp  -- power
    / Primary

  Primary
    <- "(" Expr ")"  -- paren
    / "+" Primary   -- pos
    / "-" Primary   -- neg
    / ident
    / number

  # identifier
  ident
    <- letter alnum*

  number
    <- digit* "." digit+  -- decimal
    / digit+             -- int

  alnum
    <- letter
    | digit

  letter
    <- "a".."z"
    | "A".."Z"

  digit
    <- "0".."9"

  // The `skip` rule is special, in that Syntactic rules (rules named with an uppercase first letter) will 
  // implicitly apply the skip rule 0+ times between terms, consuming any characters matched by the skip rule.
  // Characters consumed by the skip rule will be treated as non-existent by the other terms in the grammar.
  // For example, if the skip rule were defined as `skip <- " " | "\n" | "\t"`, then the following rule, 
  // `Foo = "bar" "baz"`, would match on the string "bar      \t\t\n\n    baz"", because the
  // whitespace in between the terms would be ignored.
  skip
    <- "\u0000".." "
}
```

### Labels

Labels may be applied to ordered choice alternatives, rule applications, terminals, sequences, 
optionals, zero+ repetitions, and 1+ repetitions.

Multiple terms may be labeled with the same label.

Labels and their captured terms are aggregated at the rule application level. Specifically, each rule application parse tree node
is associated with all the labels that appear in that rule. This means that any label associated with any term in the branch of the
rule specification captured by the parse tree will be available to be referenced within the context of the parse tree node representing
the application of that rule.

#### Top-level alternatives

Labels may be attached to each of a rule's top-level alternatives.
If any of the top-level alternatives are labeled, then they must all be labeled.

```
foo <- bar ("bar"? bar)+ -- label1
    / baz "123"? -- label2
    / qux -- label3
```

#### Rule applications

Labels may be attached to any rule application.

```
foo <- l1=bar l2=baz (" " l3=qux)?
```

#### Terminals

Labels may be attached to terminals.

```
foo <- l1="bar" l2="baz" (" " l3="qux")?
```

#### Optionals

Labels may be attached to optional terms.

```
foo <- bar l1=bas?
```

```
foo <- bar l1=("->" bas)?
```

#### Multiple terms with the same label

When multiple terms are labeled with the same label, any resulting match of those terms is grouped together to form a capture sequence.

```
list <- items+=item ("," items+=item)* (";" items+=item)? items+=(foo / bar / baz)
```

## Implementation Notes

Left recursion is supported via a derivation of Tratt's Algorithm 2 (see https://tratt.net/laurie/research/pubs/html/tratt__direct_left_recursive_parsing_expression_grammars/)

Tratt's Algorithm 2 is subject to the following recursion rules:
1. Non-left-recursive rules are allowed to have potential right-recursion. (Section 6.3, second to last paragraph.)
   Non-left-recursive rules can safely include potentially (direct or indirect) right-recursive calls. (Section 6.1, last paragraph).
2. Left-recursive rules must either have no right-recursion or definite right-recursion. (Section 6.3, second to last paragraph.)
   Potentially left-recursive rules may not have potential right-recursion. (Section 6.1, last paragraph.)
   Left-resursive rules may not have potential right-recursion. (Section 6.3, second to last paragraph.)

I don't understand the distinctions that Tratt makes in his definitions of potential left recursion, definite left recursion, 
left recursion, potential right recursion, definite right recursion, and right recursion well enough to fully understand the 
implication of those rules. Additionally, while trying to implement Algorithm 2, as stated, I encountered multiple issues 
with the resulting parse trees and the general runtime behavior of the parser. I didn't document each error, rather, I 
wound up tracing through the execution of the algorithm, documenting my findings of desirable behavior in the source code, 
and implementing the desired behavior.

In the end, I wound up modifying Algorithm 2 because I couldn't make Algorithm 2, as written, work. I believe Algorithm 2 
is incorrect in at least one key place, with the implication being that it's effectively broken, as stated in the paper. 
Therefore, I have attempted to generalize Tratt's approach such that it can handle direct and indirect left and right 
recursion. I believe my implementation does that. If you run into any unexpected behavior, please open an issue in Github 
and I will try to either clarify things or fix anything that's broken.