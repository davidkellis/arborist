# Overview

Test these Antlr4 grammars like this:
```bash
# setup
arborist/antlr4 ❯ export CLASSPATH=".:./antlr-4.7.2-complete.jar:$CLASSPATH"
arborist/antlr4 ❯ alias antlr4='java -jar ./antlr-4.7.2-complete.jar'
arborist/antlr4 ❯ alias grun='java org.antlr.v4.gui.TestRig'

# compile grammar
arborist/antlr4 ❯ antlr4 A1.g4
arborist/antlr4 ❯ javac A1*.java

# test grammar
arborist/antlr4 ❯ echo baa | grun A1 a -tree
line 1:3 token recognition error at: '\n'
line 2:0 no viable alternative at input '<EOF>'
(a b (a (a a) (a a)))

arborist/antlr4 ❯ echo aaaa | grun A1 a -tree
line 1:4 token recognition error at: '\n'
line 2:0 no viable alternative at input '<EOF>'
(a (a (a (a a) (a a)) (a a)) (a a))
```