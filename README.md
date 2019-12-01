# peg-parser
Parsing-Expression-Grammer-Parser-Generating Macro

The Parsing Expression Grammer (PEG) is a type of analytic formal grammar describing a formal language in terms of a set of rules
for recognizing strings in the language ([wikipedia](https://en.wikipedia.org/wiki/Parsing_expression_grammar)).
In short, this grammer is characterized by an ordered–choice operator, "/", which is an "ordered" and "short-circuit" version of
the "|" (or-operator) of a regular expression.

## Example
A following example shows an implementation of the grammer of Niklaus Wirth's PL/0 programming language
([wikipedia](https://en.wikipedia.org/wiki/Recursive_descent_parser#Example_parser))
(I assume all identifiers and numbers have been converted to symbols and number values, respectively).
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
Then, the macro `defparser` defines a function `program` that can understand above grammer.
```common-lisp
CL-USER> (program '("const" x "=" 12 ";" "begin" x ":=" 2 "end"))
T     ;; result
10    ;; "next" index position
T     ;; (partially) match-p
T     ;; input-exhausted-p
```
By default, a defuned parser returns T or an unmatch descriptor (it may depend on my preference in future).
This behavior can be changed by giving a result-interpretor function as a third element of a rule as follows.
```common-lisp
  ...
  (<factor>     (/ <ident> <num> (group '"(" <expression> '")")) ;; "group" combines results into a list
                (lambda (x) (if (consp x) (cadr x) x)))          ;; <- this!
  ...
```
In this case, the lambda function will be invoked by one of the matched result (1) `<ident>`, (2) `<num>`,
or (3) a list `("(" <expression> ")")`, and expected to return the results' interpreted value.