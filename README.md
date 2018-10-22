# arborist

Arborist is a PEG parser that supports left and right recursion.

Arborist may be used as a PEG parser combinator library or as a command line tool that parses a given input file according to a grammar file and prints the resulting parse tree.

Features
- Left recursion
- Right recursion
- May be used as a PEG parser combinator library
- May be used as a command line parser that produces a parse tree for a given input document


## Installation

Add this to your application's `shard.yml`:

```yaml
dependencies:
  arborist:
    github: davidkellis/arborist
```


## Usage

### Library

```crystal
require "arborist"

include Arborist::DSL

# e -> e - e | e + e | num
# num -> [0-9]+
e = choice(
  seq(apply("e"), term("-"), apply("e")), 
  seq(apply("e"), term("+"), apply("e")), 
  apply("num")
)
num = plus(range('0'..'9'))

matcher = Matcher.new.add_rule("e", e).add_rule("num", num)

puts "parse tree:"
puts m1.match("1-2+3-4+5", "e").try(&.syntax_tree)
```

prints

```
parse tree:
[[[[["1"], "-", ["2"]], "+", ["3"]], "-", ["4"]], "+", ["5"]]
```

### Command Line Interface

```bash
$ cat mygrammar.g
E = E - E | E + E | Num
Num = ("0".."9")+

$ cat input_document.txt
1-2+3-4+5

$ arborist -g mygrammar.g input_document.txt
[[[[["1"], "-", ["2"]], "+", ["3"]], "-", ["4"]], "+", ["5"]]
```


## Grammar Definition Syntax

### Labels

Labels be applied to top-level alternatives, rule applications, and terminals.

Multiple terms may be labeled with the same label.

#### Top-level alternatives

Labels may be attached to each of a rule's top-level alternatives.
If any of the top-level alternatives are labeled, then they must all be labeled.

```
foo = bar -- label1
    | baz -- label2
    | qux -- label3
```

#### Rule applications

Labels may be attached to any rule application.

```
foo = l1=bar l2=baz (" " l3=qux)?
```

#### Terminals

Labels may be attached to terminals.

```
foo = l1="bar" l2="baz" (" " l3="qux")?
```

#### Optionals

Labels may be attached to optional terms.

```
foo = bar l1=bas?
```

```
foo = bar l1=("->" bas)?
```

#### Multiple terms with the same label

When multiple terms are labeled with the same label, any resulting match of those terms is grouped together to form a capture sequence.

```
list = items+=item ("," items+=item)* (";" items+=item)? items+=(foo | bar | baz)
```

## Implementation Notes

Left recursion is supported via something approximating Tratt's Algorithm 2 (see https://tratt.net/laurie/research/pubs/html/tratt__direct_left_recursive_parsing_expression_grammars/)

Tratt's Algorithm 2 is subject to the following recursion rules:
1. Non-left-recursive rules are allowed to have potential right-recursion. (Section 6.3, second to last paragraph.)
   Non-left-recursive rules can safely include potentially (direct or indirect) right-recursive calls. (Section 6.1, last paragraph).
2. Left-recursive rules must either have no right-recursion or definite right-recursion. (Section 6.3, second to last paragraph.)
   Potentially left-recursive rules may not have potential right-recursion. (Section 6.1, last paragraph.)
   Left-resursive rules may not have potential right-recursion. (Section 6.3, second to last paragraph.)

I don't understand the distinctions that Tratt makes in his definitions of potential left recursion, definite left recursion, left recursion, potential right recursion, definite right recursion, and right recursion well enough to fully understand the implication of those rules; therefore, I have conservatively distilled Tratt's rules for recursion into the following:

Definitions:
- Left-recursion: a rule is left recursive if any input phrase would result in the rule being applied recursively in the left-most term position of the rule's expression. The following are left recursive: `e -> e? "foo"`, `e -> "foo"? e?`, `e -> "foo"? e*`
- Right-recursion: a rule is right recursive if any input phrase would result in the rule being applied recursively in the right-most term
position of the rule's expression. The following are right recursive: `e -> "foo" e?`, `e -> e? "foo"?`, `e -> e* "foo"?`
- Direct recursion: a rule is directly recursive if it refers to itself directly, within its own rule definition. For example: `a -> "foo" a?`
- Indirect recursion: a rule is indirectly recursive if it referring to another rule that then refers back to the original rule. Additionally, indirect recursion may pass through multiple levels of indirection. For example, in the following four rule grammar, `a -> b; b -> c; c -> d; d -> a`, all four rules, `a`, `b`, `c`, and `d` are indirectly recursive.
- Direct Definite right-recursion: a rule is directly and definitely right recursive if the rule is directly recursive, right-recursive, and the right-recursive term is static/fixed/unchanging and unconditional. In other words, the right-recursive term must not be dependent on the input being matched.

Note: I believe my definitions of left-recursion and right-recursion more closely correspond to Tratt's notion of potential left-recursion and potential-right recursion, respectively. I believe my definition of direct definite right-recursion is more restrictive than Tratt's.

Rules:
1. A left-recursive must either be (1) not right-recursive, or (2) directly and definitely right-recursive.
2. It is unknown whether indirect left-recursion will work or not, therefore, it is suggested that indirect left-recursion be avoided.
