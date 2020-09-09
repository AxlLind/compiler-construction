# Lab 3: Parser

In this lab, you'll work on the second part of the punkt0 compiler
project. Your goal is to manually implement a recursive-descent parser
to transform programs described by the [punkt0 grammar](lab2.md)
into abstract syntax trees. You also need to write a pretty-printer
for these trees. This assignment is rather long and we can only
recommend that you start early, that you make sure you understand
every step, and that you ask otherwise.

One way to partially check your implementation is to use the
pretty-printer to output the parsed AST. If you then re-parse this
output and re-pretty-print it, you should obtain exactly the same
result. In other words: for all input programs `P`, you should have
that the identity `print(parse(P)) = print(parse(print(parse(P))))`
holds. While this condition is necessary to ensure you have properly
written your parser and printer, it is not sufficient, and you are as
usual responsible for checking the details of your implementation
before you submit it.


## Parser

* Write a recursive-descent Parser for [punkt0](lab2.md) by manually
  writing mutually recursive procedures, as sketched in the lecture on
  recursive descent parsing, and generating the appropriate abstract
  syntax trees.

* Implement a new command-line option `--ast` for printing out the
  AST.

### Notes

  * It's not allowed in punkt0 to have a variable with a name that
    matches a keyword.

  * You need to properly encode the operator precedence as in Java and
    Scala. From highest priority to lowest: **!**, then **'*'** and
    **/**, then **+** and **-**, then **<** and **==**, then **&&**,
    then **||**.  Except for the unary operators, all operators are
    left-associative.  There is no precedence between operators of the
    same level. For instance: `4 / 3 * 6` reads as `((4 / 3) * 6)` and
    `6 * 4 / 3` as `((6 * 4) / 3`.

  * An `else` keyword always applies to the closest `if`. You can add
    comments to mark the end of `if` blocks in your pretty-printer to
    make sure you parse things properly.
<code java>
if (expr1) if (expr2) a = 1 else a = 2
</code>
...could be reprinted as:
<code java>
if (expr1) { if (expr2) { a = 1 } else { a = 2 } }
</code>
...and should **not** produce:
<code java>
if (expr1) { if (expr2) { a = 1 } } else { a = 2 }
</code>

  * You have to write the parser manually, as a series of mutually
    recursive functions. You are not allowed to use parser combinators
    or other such libraries.

  * You need to set the position of the trees. Normally, the position
    of the tree corresponding to `42 + 23` is the position of the `4`.

  * We provide a number of both valid and invalid test programs which
    you can use to test both your lexer and your parser (there are
    subdirectories "lab2" and "lab3").

  * The provided test programs also include the reference output for
    each test program (tokens for each valid lexer test, AST for each
    valid parser test).

## Pretty-printer

Write a pretty-printer for the AST hierarchy. Your pretty-printer
should transform any valid AST into the corresponding textual
representation in the punkt0 programming language. The output does not
need to be exactly as the input file; whitespaces can be placed
differently, comments will disappear and parentheses can be added (or
removed), as long as the program keeps the original intended
meaning. Re-parsing the pretty-printed output and re-printing it
should yield an identical result, though.

* Implement a new command-line option `--print` for pretty-printing
  the AST of a program.
