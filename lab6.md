# Labs 6: Code Generation

Congratulations, your front-end is complete! You are now one step away from having written a complete compiler. This week's lab description and stubs are rather short. It's not that we don't want to help you anymore, it's just that the tasks should be straightforward (and there's a bit of reading to do on the [Cafebabe website](https://github.com/psuter/cafebabe/wiki)).

## Support library

Generating class files is a tedious and not so interesting task. Therefore you will use a library for this. Please read the [Cafebabe documentation pages](https://github.com/psuter/cafebabe/wiki).

**Please note that** we have provided the Cafebabe jar file in folder `lib`, this version is well tested with Scala 2.12.6 and we would recommend you to directly use this one. If you are using Sbt, it will automatically import all the jar files in `lib` when compiling.

## Printing strings, booleans, and integers

To print strings you will need to call a Java library method (there are no bytecodes to do that directly). For this, you need to invoke `System.out.println(...)` of Java's standard library. `System.out` is a static field, so you first need to emit a `GETSTATIC` bytecode, then emit the code for the expression you're trying to print, then emit an `INVOKEVIRTUAL` bytecode. `System.out` is of type `java.io.PrintStream`. How to generate this particular sequence of instructions is shown in [this example](https://github.com/psuter/cafebabe/wiki/FullExamples#hello-world) on the Cafebabe wiki. If in doubt, compile an example with "javac" and run "javap -c" on the ".class" file to see what it did.

Converting a boolean or an integer to a string is done using the `java.lang.StringBuilder` class. The procedure consists in creating a new instance of `StringBuilder`, then appending to it the value you want to convert, then calling `toString()` on the builder instance. The `append` method is overloaded for booleans and integers (calling types `(Z)Ljava/lang/StringBuilder;` and `(I)Ljava/lang/StringBuilder;`), and you're not asked to be able to convert any other type (... but you are of course free to handle the conversion of arbitrary objects using `toString()`).

## Concatenating strings

Concatenating strings is done using the `java.lang.StringBuilder` class. The procedure consists in creating a new instance of `StringBuilder`, then appending to it whatever you want to concatenate together, then calling `toString()` on the builder. The `append` method is overloaded for strings and integers, and you're not asked to be able to concatenate any other type.

## Equality

We will handle equality in a simple way:

  * integers and booleans are compared by value
  * other types (strings, arrays, and objects) are compared by reference. In the case of strings, the result may or may not be the same as calling `.equals(...)`, depending on whether the strings are constant. (In other words, don't do anything special for equality.)

## Boolean expressions

You have to apply **lazy evaluation** to boolean expressions (short-circuit). Make sure your compiled code for expressions such as `(true || (1/0 == 1))` doesn't crash.

## Notes on types in the JVM

The various naming conventions in the JVM can be confusing: some bytecodes contain a letter indicating to which type of operands they apply (e.g., `ILOAD` which loads integers, `IF_ACMPEQ` which compares two references for equality, or `LRETURN` which returns a "long" value). In this convention, we have:

|  Letter  |  Corresponding type  |
|----------|----------------------|
|  I  |  Integer   |
|  L  |  Long      |
|  D  |  Double    |
|  F  |  Float     |
|  A  |  Reference (object or array)  |

...but then there is *another convention* when we want to describe field or method types and method signatures (see [this section](https://github.com/psuter/cafebabe/wiki#types-in-class-files)). In particular, note that `L` is used for objects (but not arrays) in that second convention, but for ''long'' operands in bytecodes.

Finally, note that returning from a method with return type `Unit` (a ''void'' method) is done using the `RETURN` bytecode (no prefix letter).

## Task

Complete the stub for `CodeGeneration.scala` such that your compiler emits class files. You should be able to run the main method with `java Main` and get the same result as with "scalac" for valid Punkt0 programs.

Your compiler should generate class files (one per class, as in Java, and one for the main object) silently if the compilation is successful, or generate errors and warnings from the previous phases in case there was a problem. The code generation phase should not produce any error, and the generated class files should be executable on the Java Virtual Machine. You should store all the relevant meta information in the class files: line numbers and source file identification (see the Cafebabe documentation).

Note that you should also support the `-d` option (see Main.scala) to specify the path for generating class files.

## Stubs

We provide this stub branch for your code generator component. It also contains all files of the previous stubs, plus the following new files:

  * **code/CodeGeneration.scala** (new) contains stubs for the code generator.
  * **lib/cafebabe_2.12-1.3.jar** (new) cafebabe dependancy as a jar.

## References

  * [The complete JVM specification](http://docs.oracle.com/javase/specs/jvms/se8/html/index.html) (may be hard to digest)
  * [JVM opcodes arranged conveniently](http://homepages.inf.ed.ac.uk/kwxm/JVM/index.html), in particular: [by function](http://homepages.inf.ed.ac.uk/kwxm/JVM/codeByFn.html)
  * [Cafebabe documentation](https://github.com/psuter/cafebabe/wiki)