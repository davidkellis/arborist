Grammar
  = Rule*

Rule
  = ident ruleDescr? "="  RuleBody  -- define

RuleBody
  = "|"? TopLevelTerm ("|" TopLevelTerm)*

TopLevelTerm
  = Seq caseName  -- inline
  | Seq

Alt
  = Seq ("|" Seq)*

Seq
  = Iter*

Iter
  = Pred "*"  -- star
  | Pred "+"  -- plus
  | Pred "?"  -- opt
  | Pred

Pred
  = "~" Base  -- not
  | "&" Base  -- lookahead
  | Base

Base
  = ident ~(ruleDescr? "=")                        -- application
  | oneCharTerminal ".." oneCharTerminal           -- range
  | terminal                                       -- terminal
  | "(" Alt ")"                                    -- paren

ruleDescr  (a rule description)
  = "(" ruleDescrText ")"

ruleDescrText
  = (~")" any)*

caseName
  = "--" (~"\n" space)* name (~"\n" space)* ("\n" | &"}")

name  (a name)
  = nameFirst nameRest*

nameFirst
  = "_"
  | letter

nameRest
  = "_"
  | alnum

ident  (an identifier)
  = name

terminal
  = "\"" terminalChar* "\""

oneCharTerminal
  = "\"" terminalChar "\""

terminalChar
  = escapeChar
  | ~"\\" ~"\"" ~"\n" any

escapeChar  (an escape sequence)
  = "\\\\"                                     -- backslash
  | "\\\""                                     -- doubleQuote
  | "\\\'"                                     -- singleQuote
  | "\\b"                                      -- backspace
  | "\\n"                                      -- lineFeed
  | "\\r"                                      -- carriageReturn
  | "\\t"                                      -- tab
  | "\\u" hexDigit hexDigit hexDigit hexDigit  -- unicodeEscape
  | "\\x" hexDigit hexDigit                    -- hexEscape

space
  = comment

comment
  = "//" (~"\n" any)* "\n"  -- singleLine
  | "/*" (~"*/" any)* "*/"  -- multiLine

tokens = token*

token = caseName | comment | ident | operator | punctuation | terminal | any

operator = "=" | "*" | "+" | "?" | "~" | "&"

punctuation = "<" | ">" | "," | "--"