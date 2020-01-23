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


## Build Command Line Interface
```bash
$ crystal build -o arborist src/cli.cr
$ ls -al ./arborist
-rwxr-xr-x@ 1 david  staff  3095952 Nov  2 14:26 ./arborist
```

Alternatively, just run `./build.sh`, which does the same thing as above.


## Run Specs
```bash
~/Dropbox/projects/arborist master *1 !1 ?2 ❯ crystal spec
...........................*.......................

Pending:
  Arborist left-recursion support never matches any phrase for the rule: a = !a 'b' ; see https://github.com/harc/ohm/issues/120

Finished in 3.29 seconds
51 examples, 0 failures, 0 errors, 1 pending
```


## Profiling

### macOS

#### Instruments

Following the instructions captured in http://www.mikeperham.com/2016/06/24/profiling-crystal-on-osx/:
1. Install Xcode
2. Make sure you can run the Instruments tool from within Xcode, as described in https://help.apple.com/instruments/mac/current/#/devc1724975
3. Do the following:
   ```bash
   $ instruments -t "Time Profiler" ./arborist -s -g src/grammar.arborist src/grammar.arborist
   # OR
   # $ instruments -l 30000 -t "Time Profiler" ./arborist -s -g src/grammar.arborist src/grammar.arborist     # 30 seconds of profiling
   # $ instruments -l 120000 -t "Time Profiler" ./arborist -s -g src/grammar.arborist src/grammar.arborist     # 2 mins of profiling
   # $ instruments -l 300000 -t "Time Profiler" ./arborist -s -g src/grammar.arborist src/grammar.arborist     # 5 mins of profiling
   Instruments Trace Complete: ~/projects/arborist/instrumentscli0.trace
   $ open instrumentscli0.trace
   # Examine the trace from within the Instruments tool
   ```

#### dtrace & FlameGraph

Following the instructions captured in https://carol-nichols.com/2017/04/20/rust-profiling-with-dtrace-on-osx/:
```bash
$ cd arborist
$ sudo dtrace -c './arborist -s -g src/grammar.arborist src/grammar.arborist' -o out.stacks -n 'profile-997 /execname == "arborist"/ { @[ustack(100)] = count(); }'
...
$ cd ..
$ git clone https://github.com/brendangregg/FlameGraph.git
Cloning into 'FlameGraph'...
remote: Enumerating objects: 25, done.
remote: Counting objects: 100% (25/25), done.
remote: Compressing objects: 100% (20/20), done.
remote: Total 1063 (delta 11), reused 13 (delta 5), pack-reused 1038
Receiving objects: 100% (1063/1063), 1.88 MiB | 4.19 MiB/s, done.
Resolving deltas: 100% (606/606), done.
$ cd FlameGraph
$ ./stackcollapse.pl ../arborist/out.stacks | ./flamegraph.pl > ~/Downloads/arborist_flamegraph.svg
# Examine the FlameGraph plot in a web browser (e.g. Firefox) or some other SVG file viewer
```

## Usage

### Command Line Interface

```bash
~/projects/arborist ❯ ./build.sh
~/projects/arborist ❯ cat mygrammar.g
Start <- E !.
E <- E "-" E / E "+" E / ("0".."9")+
~/projects/arborist ❯ cat input_document.txt
1-2+3-4+5
~/projects/arborist ❯ ./arborist --simple -g mygrammar.g input_document.txt
(Start (E (E (E (E (E 1) - (E 2)) + (E 3)) - (E 4)) + (E 5)))
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

```bash
~/projects/arborist ❯ cat test.cr
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
m1 = Arborist::Matcher.new.add_rule("e", e).add_rule("num", num)

# parse an expression into a parse tree

parse_tree = m1.match("1-2+10-3+10", "e")

puts "expression: 1-2+10-3+10"
puts "parse tree:"
puts parse_tree.try(&.simple_s_exp)

# create a visitor to traverse the parse tree

eval = Arborist::Visitor(Int32).new

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

~/projects/arborist ❯ crystal run test.cr
expression: 1-2+10-3+10
parse tree:
(e (e (e (e (e (num 1)) - (e (num 2))) + (e (num 1 0))) - (e (num 3))) + (e (num 1 0)))
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

## Example Grammars

In the `examples` directory, there are sample grammars that may be used like the following examples demonstrate:

Check to see if the JavaScript grammar, `examples/es5.arborist`, recognizes the sample javascript file `examples/es5_example_uglifyjs2.js`:
```
~/projects/arborist ❯ ./arborist -m simple -r -t -g examples/es5.arborist examples/es5_example_uglifyjs2.js
timings: load grammar = 00:00:04.522036000 ; parse = 00:00:05.037073000
Input conforms to grammar.
```

Parse `examples/es5_example_hello_world.js` with the JavaScript grammar and print the corresponding simplified s-expression parse tree:
```
~/projects/arborist ❯ ./arborist -m simple --simple -t -g examples/es5.arborist examples/es5_example_hello_world.js
timings: load grammar = 00:00:04.838410000 ; parse = 00:00:00.011605000
(Start (__ ) (Program (SourceElements (SourceElement (FunctionDeclaration (FunctionToken function) (__ (WhiteSpace )) (Identifier (IdentifierName (IdentifierStart (UnicodeLetter (Ll (unicode_lower h)))) (IdentifierPart (IdentifierStart (UnicodeLetter (Ll (unicode_lower e))))) (IdentifierPart (IdentifierStart (UnicodeLetter (Ll (unicode_lower l))))) (IdentifierPart (IdentifierStart (UnicodeLetter (Ll (unicode_lower l))))) (IdentifierPart (IdentifierStart (UnicodeLetter (Ll (unicode_lower o))))))) (__ ) ( (__ ) (FormalParameterList (Identifier (IdentifierName (IdentifierStart (UnicodeLetter (Ll (unicode_lower w)))) (IdentifierPart (IdentifierStart (UnicodeLetter (Ll (unicode_lower o))))) (IdentifierPart (IdentifierStart (UnicodeLetter (Ll (unicode_lower r))))) (IdentifierPart (IdentifierStart (UnicodeLetter (Ll (unicode_lower l))))) (IdentifierPart (IdentifierStart (UnicodeLetter (Ll (unicode_lower d)))))))) (__ ) ) (__ (WhiteSpace )) { (__ (LineTerminatorSequence ) (WhiteSpace ) (WhiteSpace )) (FunctionBody (SourceElements (SourceElement (Statement (ReturnStatement (ReturnToken return) (_ (WhiteSpace )) (Expression (AssignmentExpression (ConditionalExpression (LogicalORExpression (LogicalANDExpression (BitwiseORExpression (BitwiseXORExpression (BitwiseANDExpression (EqualityExpression (RelationalExpression (ShiftExpression (AdditiveExpression (MultiplicativeExpression (UnaryExpression (PostfixExpression (LeftHandSideExpression (NewExpression (MemberExpression (PrimaryExpression (Literal (StringLiteral " (DoubleStringCharacter (SourceCharacter h)) (DoubleStringCharacter (SourceCharacter e)) (DoubleStringCharacter (SourceCharacter l)) (DoubleStringCharacter (SourceCharacter l)) (DoubleStringCharacter (SourceCharacter o)) "))))))))) (__ (WhiteSpace )) (AdditiveOperator +) (__ (WhiteSpace )) (MultiplicativeExpression (UnaryExpression (PostfixExpression (LeftHandSideExpression (NewExpression (MemberExpression (PrimaryExpression (Identifier (IdentifierName (IdentifierStart (UnicodeLetter (Ll (unicode_lower w)))) (IdentifierPart (IdentifierStart (UnicodeLetter (Ll (unicode_lower o))))) (IdentifierPart (IdentifierStart (UnicodeLetter (Ll (unicode_lower r))))) (IdentifierPart (IdentifierStart (UnicodeLetter (Ll (unicode_lower l))))) (IdentifierPart (IdentifierStart (UnicodeLetter (Ll (unicode_lower d)))))))))))))))))))))))))) (EOS (__ ) ;)))))) (__ (LineTerminatorSequence )) })) (__ (LineTerminatorSequence ) (LineTerminatorSequence )) (SourceElement (Statement (ExpressionStatement (Expression (AssignmentExpression (ConditionalExpression (LogicalORExpression (LogicalANDExpression (BitwiseORExpression (BitwiseXORExpression (BitwiseANDExpression (EqualityExpression (RelationalExpression (ShiftExpression (AdditiveExpression (MultiplicativeExpression (UnaryExpression (PostfixExpression (LeftHandSideExpression (CallExpression (MemberExpression (PrimaryExpression (Identifier (IdentifierName (IdentifierStart (UnicodeLetter (Ll (unicode_lower c)))) (IdentifierPart (IdentifierStart (UnicodeLetter (Ll (unicode_lower o))))) (IdentifierPart (IdentifierStart (UnicodeLetter (Ll (unicode_lower n))))) (IdentifierPart (IdentifierStart (UnicodeLetter (Ll (unicode_lower s))))) (IdentifierPart (IdentifierStart (UnicodeLetter (Ll (unicode_lower o))))) (IdentifierPart (IdentifierStart (UnicodeLetter (Ll (unicode_lower l))))) (IdentifierPart (IdentifierStart (UnicodeLetter (Ll (unicode_lower e)))))))) (__ ) . (__ ) (IdentifierName (IdentifierStart (UnicodeLetter (Ll (unicode_lower l)))) (IdentifierPart (IdentifierStart (UnicodeLetter (Ll (unicode_lower o))))) (IdentifierPart (IdentifierStart (UnicodeLetter (Ll (unicode_lower g))))))) (__ ) (Arguments ( (__ ) (ArgumentList (AssignmentExpression (ConditionalExpression (LogicalORExpression (LogicalANDExpression (BitwiseORExpression (BitwiseXORExpression (BitwiseANDExpression (EqualityExpression (RelationalExpression (ShiftExpression (AdditiveExpression (MultiplicativeExpression (UnaryExpression (PostfixExpression (LeftHandSideExpression (CallExpression (MemberExpression (PrimaryExpression (Identifier (IdentifierName (IdentifierStart (UnicodeLetter (Ll (unicode_lower h)))) (IdentifierPart (IdentifierStart (UnicodeLetter (Ll (unicode_lower e))))) (IdentifierPart (IdentifierStart (UnicodeLetter (Ll (unicode_lower l))))) (IdentifierPart (IdentifierStart (UnicodeLetter (Ll (unicode_lower l))))) (IdentifierPart (IdentifierStart (UnicodeLetter (Ll (unicode_lower o))))))))) (__ ) (Arguments ( (__ ) (ArgumentList (AssignmentExpression (ConditionalExpression (LogicalORExpression (LogicalANDExpression (BitwiseORExpression (BitwiseXORExpression (BitwiseANDExpression (EqualityExpression (RelationalExpression (ShiftExpression (AdditiveExpression (MultiplicativeExpression (UnaryExpression (PostfixExpression (LeftHandSideExpression (NewExpression (MemberExpression (PrimaryExpression (Literal (StringLiteral " (DoubleStringCharacter (SourceCharacter w)) (DoubleStringCharacter (SourceCharacter o)) (DoubleStringCharacter (SourceCharacter r)) (DoubleStringCharacter (SourceCharacter l)) (DoubleStringCharacter (SourceCharacter d)) "))))))))))))))))))))) (__ ) ))))))))))))))))))) (__ ) ))))))))))))))))))) (EOS (__ ) (EOF ))))))) (__ ))
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


## Todo

- make sure grammar rules *must* have some kind of body; they can't be blank; currently the parser builder can be tricked into allowing a blank rule if the rule "body" is a comment, e.g. MyRule <-   # some comment goes here
- implement some kind of non-terminating evaluation logic to identify when a grammar evaluation will never terminate, and report on the termination failure
  - make sure we don't end up in in an infinite loop via a Repetition or RepetitionOnePlus operator, as in the following:
```
|  finishing seed growth for rule SourceElement at 2 : '(Statement (DebuggerStatement ))'
|  matched SourceElement (call 5021071072) at 2 : '(SourceElement (Statement (DebuggerStatement )))'
|  memoizing SourceElement at 2-2 with rule_in_recursion_call_stack_state=[] : '(SourceElement (Statement (DebuggerStatement )))'
|  try apply __skip_prior_to_expr_4748386368 at 2
|  found memoized apply __skip_prior_to_expr_4748386368 at 2 : (__skip_prior_to_expr_4748386368 )
|  try apply SourceElement at 2
|  found memoized apply SourceElement at 2 : (SourceElement (Statement (DebuggerStatement )))
|  try apply __skip_prior_to_expr_4748386368 at 2
|  found memoized apply __skip_prior_to_expr_4748386368 at 2 : (__skip_prior_to_expr_4748386368 )
|  try apply SourceElement at 2
|  found memoized apply SourceElement at 2 : (SourceElement (Statement (DebuggerStatement )))
|  try apply __skip_prior_to_expr_4748386368 at 2
|  found memoized apply __skip_prior_to_expr_4748386368 at 2 : (__skip_prior_to_expr_4748386368 )
|  try apply SourceElement at 2
|  found memoized apply SourceElement at 2 : (SourceElement (Statement (DebuggerStatement )))
|  try apply __skip_prior_to_expr_4748386368 at 2
|  found memoized apply __skip_prior_to_expr_4748386368 at 2 : (__skip_prior_to_expr_4748386368 )
|  try apply SourceElement at 2
|  found memoized apply SourceElement at 2 : (SourceElement (Statement (DebuggerStatement )))
```