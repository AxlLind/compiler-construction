# Lab 4: Name Analysis

In this lab you will add name analysis to your Punkt0 compiler. This
will considerably ease the task of type checking that you will start
afterwards. Note that the type checker assignment will be released
soon, so make sure you start working on the name analysis as early as
possible.

The description of this assignment is rather long. Don't panic! :)
Most of it is to help you avoid forgetting important points, and we
give you a substantial amount of code.

## Symbols

The goal of name analysis is twofold: we want to reject programs which
contain certain types of errors, and we want to associate *symbols* to
all identifiers.

Symbols are values which uniquely identify all class, method and
variable names in a program, by mapping their (multiple) *occurrences*
to their (unique) *definition*. Identifiers are already present in the
AST and contain names as well, but these are not sufficient to
distinguish between a class member and a local variable with the same
name, for instance. In this lab we will--among other things--add this
missing information to the AST.

In the process of mapping occurrences to definitions, we will be able
to detect the following kinds or errors:

  * a class is defined more than once
  * a variable is defined more than once
  * a class member is overloaded (forbidden in Punkt0)
  * a method is overloaded (forbidden in Punkt0)
  * a method argument is shadowed by a local variable declaration
    (forbidden in Java, and we follow this convention)
  * a method argument is reassigned (in Scala method parameters are val
    and although we do not have val in punkt0 we will follow this convention)
  * two method arguments have the same name
  * a class name is used as a symbol (as parent class or type, for
    instance), but is not declared
  * an identifier is used as a variable, but is not declared
  * the inheritance graph has a cycle (e.g., `class A extends B {} class
    B extends A {}`)
  * (Note that name analysis does not check that method calls
    correspond to methods defined in the proper class. We will need
    type checking for this.)


## Implementation

In order to attach symbols to trees, we define a new trait,
`Symbolic`, and a new set of classes for the symbols. The `Symbolic`
trait is parametrized by a class name which allows us to define the
kind of symbols which can be attached to each kind of AST node (see
`Symbols.scala` and `Trees.scala` later for examples).

You need to write your analyzer such that two nodes referencing the
same symbol have the same symbol class __instance__ attached to them
(that is, reference equality, structural equality is not enough). We
defined the `Symbol` class such that symbols automatically get a
unique identifier attached to them at creation. This will allow you to
check that you are attaching symbols correctly: you will add an option
to your pretty-printer to be able to print these unique numbers along
with the identifiers where they occur.

Note that `Symbol`s are also `Positional` objects, which means you
have to set them a correct position. The position of the symbol should
be its declaration position. This is necessary to produce meaningful
error messages such as `"error: class Cl is defined twice. First
definition here: ..."`.

### Internal errors

When your compiler encounters an *internal* error (for example, a
scope is not initialized as you expected, a symbol is `null`, etc.),
you should **not** use the methods from the reporter trait. You must
use `sys.error` instead, which will throw an exception and show you
the stack trace. The reason is that you shouldn't blame the user for
internal errors. In fact, the user should never encounter an internal
error. Of course, writing bug-free programs is hard...


### Symbols as scopes

We will take advantage of the fact that scopes in Punkt0 are only of
three kinds:

  - the global scope (the set of declared classes, including a
    synthetic class for the main method)
  - the class scopes (all members and methods within a class, plus the
    global scope)
  - the method scopes (the parameters and local variables, plus the
    corresponding class scope)

This in fact defines a hierarchy among symbols:

  * all class symbols are defined in the global scope
  * all methods are defined in a class scope
  * variables are defined either in a method scope, as parameters or
    locals, or in a class scope

We encoded this hierarchy in the symbol classes. Therefore, if we have
access to a class symbol, for instance, and all symbols were correctly
set, we can access from there all method symbols and member variable
symbols. This will allow us to easily check if a variable was
declared, for instance.

### Two phases

Here is how we recommend you proceed for the implementation:

  - First, collect all symbols: create the symbol class instances, and
    attach them to field types, method member types, formal parameter
    types and method return types.

  - Make the appropriate changes to your pretty-printer and make sure
    you see the unique IDs next to the identifiers at the definition
    points.

  - Implement the second phase of your analyzer which consists in
    attaching the proper symbol to the occurrences of the
    identifiers. To simplify your task, start by writing `lookup*`
    methods in the symbol classes: they will allow you to easily check
    whether an identifier was declared and to recover its symbol if it
    was. Make sure you properly encode the scope rules (including
    shadowing) in your `lookup*` methods.

  - You can use your pretty-printer to make sure you attached symbols
    correctly.

  - Make sure that you throw errors and warnings when appropriate.


### Execution example

When analyzing the following file:

    class B extends A {
      override def foo(): Int = {
        value = 42;
        value
      }
    }

    class A {
      var value: Int = 0;
      def foo(): Int = {
        var value: Boolean = true;
        value = false;
        41
      }
    }

    object Main extends App {
      println(new B().foo())
    }


The pretty-printer would output something like (if the `--symid`
command-line option is provided):

    class B#1 extends A#2 {

      override def foo#3(): Int = {
        value#4 = 42;
        value#4
      }

    }

    class A#2 {
      var value#4: Int = 0;

      def foo#5(): Int = {
        var value#6: Boolean = true;
        value#6 = false;
        41
      }

    }

    object Main#7 extends App#?? {
      println(new B#1().foo#??())
    }

Note that:

  * Overriding methods have a different symbol than their overridden
    counterparts.

  * For now you can let methods have unresolved symbols, as we need to
    figure out the type of the object before we can figure out the
    right method signature. You will attach ids to methods in the
    next lab when we do type checking.

### Constraints

Here are all the constraints that your analyzer should enforce (note
that this is simply a reformulation of the types of errors we want to
catch):

#### Variable declarations

  * No two variables can have the same name in the same scope, unless
    one of the two cases of shadowing occurs.
  * All used variables must be declared.
  * The initializer expression in a variable or field declaration must
    be either a constant (including `null`) or a `new` expression
    (instance creation). *Note:* you can implement this constraint by
    modifying your parser to incorporate this restriction. (An
    alternative is to enforce this constraint directly in the name
    analyzer.)
  * method parameters can not be reassigned. I.e. they can not occur
    as left hand side of an assignment

#### Shadowing

Shadowing can occur in two different situations:

  - a local variable in a method can shadow a class member
  - a method parameter can shadow a class member

All other types of shadowing are not allowed in Punkt0.

#### Classes

  * Classes must be defined only once.
  * When a class is declared as extending another one, the other class
    must be declared.
  * The transitive closure of the `extends` relation must be irreflexive
    (no cycles in the inheritance graph).
  * When a class name is used as a type, the class must be declared.

#### Overloading

  * Overloading is not permitted:
    * In a given class, no two methods can have the same name.
    * In a given class, no method can have the same name as another
      method defined in a super class, unless overriding applies.

#### Overriding

  * A method in a given class overrides another one in a super class
    if they have the same name and the same number of arguments. (Of
    course this constraint will be tightened once we start checking
    types.) An overriding method must have an `override` modifier.
  * Fields cannot be overridden.

## Grading

For grading you need to implement command-line option `--symid` according to the above.
Examples can be found in the testprograms folder. And note that Lab4 and Lab5 will be graded together.
