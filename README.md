# peg-parser
Parsing-Expression-Grammer-Parser-Generating Macro

The [Parsing Expression Grammer (PEG)](https://en.wikipedia.org/wiki/Parsing_expression_grammar) is a type of analytic formal grammar describing a formal language in terms of a set of rules for recognizing strings in the language.
In short, this grammer is characterized by an ordered–choice operator, "/", which is an "ordered" and "short-circuit" version of
the "|" (or-operator) of a regular expression.

## Example
A following example shows an implementation of the grammer of Niklaus Wirth's [PL/0 programming language](https://en.wikipedia.org/wiki/PL/0) (assume all identifiers and numbers have been converted to symbols and number values, respectively).
```common-lisp
(peg:defparser program (<block>)
  (<block>      ((? ('"const" <ident> '"=" <num> (repeat '"," <ident> '"=" <num>) '";"))
                 (? ('"var"   <ident> (repeat '"," <ident>) '";"))
                 (repeat '"procedure" <ident> '";" <block> '";")
                 <statement>))
  (<statement>  (/ (<ident> '":=" <expression>)
                   ('"call"  <ident>)
                   ('"begin" <statement> (repeat '";" <statement>) '"end")
                   ('"if"    <condition> '"then" <statement>)
                   ('"while" <condition> '"do"   <statement>)))
  (<condition>  (/ ('"odd" <expression>)
                   (<expression> (/ '"=" '"#" '"<" '"<=" '">" '">=") <expression>)))
  (<expression> ((? (/ '"+" '"-")) <term> (repeat (/ '"+" '"-") <expression>)))
  (<term>       (<factor> (repeat (/ '"*" '"/") <factor>)))
  (<factor>     (/ <ident> <num> ('"(" <expression> '")")))
  (<ident>      '(symbolp))
  (<num>        '(numberp)))
```
Then, the macro `defparser` defines a function `program` that can understand above grammer,
```common-lisp
CL-USER> (program '("const" x "=" 12 ";" "begin" x ":=" 2 "end"))
T     ;; result
10    ;; "next" index position
T     ;; (partially) match-p
T     ;; input-exhausted-p
```
(note that the code is syntactically okay, but it is semantically nonsense).

See also [a more complecated version](PL-0.lisp) that compiles this PL/0 to common lisp.

## Let operator (experimental)
The parser can bind a matched result to a symbol (in other words, a local non-terminal symbol) by a `let` operator. 
The below parser `tagged-tree` matches to an xml-like tagged tree with any tag-names.
```common-lisp
(peg:defparser tagged-tree (<root>)
    (<root>   (repeat <tagged-branch>)
              #'list)
    (<tagged-branch> ((_ "<")
                      (let (x <tag>)
                        (_ ">")
                        <root>
                        (_ "</") x (_ ">")))
                     (lambda (x1 r x2) `(,@x1 ,@r ,@x2)))
    (<tag> (group '(alpha-char-p) (repeat '(alpha-char-p)))
           (lambda (x) (concatenate 'string x))))
```
```common-lisp
CL-USER> (tagged-tree "<a><b></b></a>")
(("a" ("b" "b") "a"))
14
T
T
CL-USER> (tagged-tree "<a><b></c></a>")
NIL
0
T
NIL
```
