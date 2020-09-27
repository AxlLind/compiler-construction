# Lab 5: Type Checking

In this lab you will implement type checking in your Punkt0
compiler. After this step, you will have completed the front-end of
your compiler. This means that it will be able to reject *all* invalid
programs, and accept *all* valid programs. You will then be able to
turn these valid programs into assembly code that runs on the JVM,
just like "javac" and "scalac" do.

## Type checking

A valid Punkt0 program has the following properties:

* It follows the Punkt0 concrete syntax.
* It respects all the constraints mentioned in [Lab 4](lab4.md).
* Method overriding respects some typing constraints:
  * The overriding method must have exactly as many parameters as the overridden one.
  * The types of the parameters in the overriding and overridden methods must match exactly (no contravariance allowed).
  * The return type must match exactly (no covariance allowed).
  * The overriding method must carry the `override` modifier.
  * **Note:** The names of parameters may change, only the signature is to be checked.
* All expressions typecheck and have the expected type (the returned expression matches the declared return type, for instance).

Your goal in this assignment is to enforce all the constraints not
enforced already by the previous phases.

**Note:** The language and the type rules presented in the course may
differ from the rules of Punkt0. If there are any differences, please
use the description on the current page for your implementation, and
not the rules in the lecture. Of course, feel free to clarify with
us if you have any questions.

## Types

The following primitive types exist in Punkt0 (note that we prefix them
with **T** to differentiate them from the tree nodes with the same
name, for instance):

  * **TBoolean**
  * **TInt**
  * **TString** (We consider **String** to be a primitive type, unlike in Java where it is a proper class. No methods can be called on **String**s, for instance.)
  * **TUnit**

Additionnally, we have class types:

  * **AnyRef**
  * **TClass[*name*]**
  * **TNull**

We define a subtyping relation on these types. All primitive types are
subtypes of themselves and of no other type. For instance:

  * **TInt <: TInt**

All class types are subtypes of themselves and the special **AnyRef**
class type. The subtyping relation is also transitive.

  * **TClass[*name*] <: TClass[*name*]** and **TClass[*name*] <: TClass[**AnyRef**]**
  * **TClass[**B**] <: TClass[**A**]** and **TClass[**C**] <: TClass[**B**]** implies **TClass[**C**] <: TClass[**A**]**

**TNull** is subtype of all classes including **AnyRef**.

  * **TNull <: TClass[*name*]**
  * **TNull <: AnyRef**

With this in mind, we give some of the non-trivial typing
constraints. This is naturally not an exhaustive list of what you
should implement, but we expect you to be able to deduce the other
rules unambiguously yourself (if in doubt about a rule, ask on KTH
canvas).

### Overloaded `+`

The `+` operator can represent integer addition, or string
concatenation. If the types of **e**1 and **e**2 are **T**1 and **T**2
respectively, we have for the type **T**s of **e**1 + **e**2:

  * **T**1 = **TInt** and **T**2 = **TInt** implies **T**s = **TInt**
  * **T**1 = **TString** and **T**2 = **TInt** implies **T**s = **TString**
  * **T**1 = **TInt** and **T**2 = **TString** implies **T**s = **TString**
  * **T**1 = **TString** and **T**2 = **TString** implies **T**s = **TString**

All other values for **T**1 and **T**2 should result in type errors.

### Comparison operator

The `==` operator is also overloaded. Expression `e1 == e2` is type
correct if and only if one of the following two cases applies:

  * `e1` and `e2` have both primitive types, and these types are equal
  * `e1` and `e2` have both class types (in which case they can be different classes)

Note that it is **not** type correct to compare a primitive type to a
class type. Again, strings are considered **primitive**.

Consider the following code.

    class A {}
    class B {}

Let `e1: T1` and `e2: T2`. Then the following table summarizes some of the cases for `e1 == e2`.

T1     | T2     | type checks?
--     | --     | ------------
Int    | Int    | yes
String | String | yes
String | A      | no
A      | Int    | no
A      | B      | yes

### Method calls

The dereferenced object must be of a class type, and its class must
declare or inherit the called method. The number of arguments must of course
match the number of parameters. The passed arguments must have
subtypes of the declared parameters (matching one-by-one).

### Assignment

Assignment of an expression of type **T** can only be done to a
variable of type **S** such that **T <: S**.

The type of an assignment expression is **Unit**.

It is not allowed to reassign method parameters.

### This

`this` is always considered to carry the class type corresponding to
the class where it occurs.


### Returned expression

The returned expression must be of a subtype of the declared return
type.


### The `println` expression

We will consider `println` calls to be type correct if the argument has type **String**, **Int**, or **Boolean**. The type of a `println` expression is **Unit**.


### The `while` expression

The type of a **while** expression is **Unit**. Its conditional
expression must have type **Boolean**, and its body must have type
**Unit**.


### The `if` expression

The type of an `if` expression is the least upper bound of the types of the two branches. Its conditional expression must have type **Boolean**.

### The block expression

The type of a block expression is the type of the block's last expression.

### The main declaration

The main `object` declaration must extend the built-in `App`
type. (This is important to ensure Punkt0 is a subset of Scala.)


### Variable declarations

The constant initial expression must be of the correct, declared type.

## Suggested implementation

Here are the steps we suggest you take:

* Modify your analyzer such that it attaches types to the various
  symbols. Since symbols are shared, this has the advantage that you
  can recover the proper type from any occurrence of the symbol.

* Modify your analyzer so that it enforces the overriding type
  constraints on methods.

* Implement your typechecker. Make sure you attach the types to the
  expression subtrees (this will be required for code generation, to
  differentiate between the versions of the overloaded operators, for
  instance).

* While you typecheck expressions, attach the proper symbols to the
  occurrences of method calls (since you can now determine the class
  from which they are called; though, what you are able to determine
  may not match the run-time types).

* Test, test, test!

## User-friendliness

It is very important that your compiler does not stop at the first
type error! `TypeChecker.scala` contains some hints on how to achieve
this. Detect as many errors as possible!

## Grading

For grading you need to produce an AST that annotates each identifier with a
unique ID and each node that is a subclass of ExprTree with the type of that
expression given the `--ast+` option. Examples can be found in the testprograms
folder. If you are using the classes as provided by the stubs you can use
`analyzer.TypedASTPrinter` for printing the anotated AST.

Note: after typechecking the methods should have an attached id when printing with
`--symid` or `--ast+`. For grading, please use the id of the class it self if it
defines the method or otherwise the id of the first super class that defines the
method.
