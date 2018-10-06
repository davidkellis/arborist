# pegparser

A PEG parser combinator in Crystal.

Left recursion is supported via Tratt's Algorithm 2: https://tratt.net/laurie/research/pubs/html/tratt__direct_left_recursive_parsing_expression_grammars/

The rules for left recursion are:
1. Non-left-recursive rules are allowed to have potential right-recursion.
2. Left-recursive rules must either have no right-recursion or definite right-recursion.
3. Potentially left-recursive rules may not have potential right-recursion.

## Installation

Add this to your application's `shard.yml`:

```yaml
dependencies:
  pegparser:
    github: davidkellis/pegparser
```

## Usage

```crystal
require "pegparser"
```
