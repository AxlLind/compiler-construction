# Lab 2: Lexer

This assignment is the first real part of the course's compiler
project. Your task is to write the lexer for your compiler. Your
compiler should compile programs written in a simple, but nonetheless
interesting, object-oriented programming language. The language is
called "Punkt0" in reference to Scala's core calculus,
[DOT](https://infoscience.epfl.ch/record/215280). The BNF grammar of
Punkt0 follows. In addition, below you are given additional
specifications for the token classes `<IDENTIFIER>`,
`<INTEGER_LITERAL>`, `<STRING_LITERAL>`, and `<EOF>`. Here are some
details you should pay attention to:

* Make sure you recognize keywords as their own token type. The
  keyword `while`, for instance, should be lexed as the token type
  `WHILE`, not as an identifier representing an object called `while`.

* Make sure you correctly register the position of all tokens. Note
  the `Source.pos` and the `Positioned.setPos` methods.

* In general, it is good to output as many errors as possible (this
  helps whoever uses your compiler). For instance, if your lexer
  encounters an invalid character, it can output an error message,
  skip it, and keep on lexing the rest of the input. After lexing, the
  compiler still won't proceed to the next phase, but this helps the
  user correct more than one error per compilation. Use the special
  `BAD` token type to mark errors and keep lexing as long as it is
  possible.

* The Lexer does not immediately read and return all tokens, it
  returns an `Iterator[Token]` that will be used by future phases to
  read tokens on demand.

* Comments should not produce tokens.

* Comments in Punkt0 can be marked using the end-of-line notation
  (`//`) or blocks (`/* .. */`). Nested block comments are **not
  allowed**.

* Your lexer should support a command-line option `--tokens` for
  printing out the tokens that were recognized. With this option, your
  compiler should output one token per line. After all tokens have
  been recognized, your compiler should exit.

* Your compiler should terminate with exit code `1` in case the lexer
  detected errors. See method `Reporter.terminateIfErrors`.

## Code stubs

We provide code stubs for your project. Your task is to complete the
file `Lexer.scala` and to implement the above `--tokens` option in
`Main.scala`.

| Non-terminal        |     | Definition                                                                                                                                                                                    |
| ------------------- | --- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| *Program*           | ::= | ( *ClassDeclaration* ) * *MainDeclaration* <EOF>                                                                                                                                              |
| *ClassDeclaration*  | ::= | **class** *Identifier* ( **extends** *Identifier* )? **{** ( *VarDeclaration* ) * ( *MethodDeclaration* ) * **}**                                                                             |
| *MainDeclaration*   | ::= | **object** *Identifier* **extends** *Identifier* **{** ( *VarDeclaration* ) * *Expression* ( **;** *Expression* ) * **}**                                                  |
| *VarDeclaration*    | ::= | **var** *Identifier* **:** *Type* **=** *Expression* **;**                                                                                                                                    |
| *MethodDeclaration* | ::= | ( **override** )? **def** *Identifier* **(** ( *Identifier* **:** *Type* ( **,** *Identifier* **:** *Type* ) * )? **)** **:** *Type* = **{** ( *VarDeclaration* ) * *Expression* ( **;** *Expression* ) * **}** |
| *Type*              | ::= | **Boolean**                                                                                                                                                                                   |
|                     |     | **Int**                                                                                                                                                                                       |
|                     |     | **String**                                                                                                                                                                                    |
|                     |     | **Unit**                                                                                                                                                                                      |
|                     |     | *Identifier*                                                                                                                                                                                  |
| *Expression*        | ::= | *Expression* ( **&&** &#124; **&#124;&#124;** &#124; **==** &#124; **<** &#124; **+** &#124; **-** &#124; **&#42;** &#124; **/** ) *Expression*                                                                                              |
|                     |     | *Expression* **.** *Identifier* **(** ( Expression ( **,** Expression ) * )? **)**                                                                                                            |
|                     |     | `<INTEGER_LITERAL>`                                                                                                                                                                           |
|                     |     | **"** `<STRING_LITERAL>` **"**                                                                                                                                                                |
|                     |     | **true**                                                                                                                                                                                      |
|                     |     | **false**                                                                                                                                                                                     |
|                     |     | *Identifier*                                                                                                                                                                                  |
|                     |     | **this**                                                                                                                                                                                      |
|                     |     | **null**                                                                                                                                                                                      |
|                     |     | **new** *Identifier* **(** **)**                                                                                                                                                              |
|                     |     | **!** *Expression*                                                                                                                                                                            |
|                     |     | **(** *Expression* **)**                                                                                                                                                                      |
|                     |     | **{** ( *Expression* ( **;** *Expression* ) * )? **}**                                                                                                                                        |
|                     |     | **if** **(** *Expression* **)** *Expression* ( **else** *Expression* )?                                                                                                                       |
|                     |     | **while** **(** *Expression* **)** *Expression*                                                                                                                                               |
|                     |     | **println** **(** *Expression* **)**                                                                                                                                                          |
|                     |     | *Identifier* **=** *Expression*                                                                                                                                                               |
| *Identifier*        | ::= | `<IDENTIFIER>`                                                                                                                                                                                |


Special token classes:

* `<IDENTIFIER>` represents a sequence of letters, digits and
  underscores, starting with a letter and which is not a
  keyword. Identifiers are case-sensitive.

* `<INTEGER_LITERAL>` represents a sequence of digits, with no leading
  zeros.

* `<STRING_LITERAL>` represents a sequence of arbitrary characters,
  except new lines and **"**. We don't ask you to support escape
  characters such as **\n**.

* `<EOF>` represents the special end-of-file character.
