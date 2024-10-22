# A complete compiler in Scala
This repo contains the compiler I created in the course [Compiler Construction](https://www.kth.se/student/kurser/kurs/DD2488?l=en) at KTH, completely written in Scala. The course involved you creating a complete compiler for a simple, statically typed, garbage collected language targeting the JVM. This included all steps of a classic compiler structure:
- Lexing
- Parsing
- Name Analysis
- Type Checking
- Code generation (JVM bytecode)

This compiler along with the individual project (see below) earned me an `A` in the course. While simple, this is a real, somewhat usable language targeting the JVM. See below for an example program!

## Individual project - A garbage collected C-backend from scratch
The course concluded with an individual project where you implemented a *non-trivial* extension to the compiler. You were free to propose any extension you wanted. For my project I implemented an alternative backend to the compiler. This backend outputs C code instead of JVM bytecode, which can then be compiled with any C compiler. Due to the language supporting inheritance, garbage collection, and if/block-expressions this was quite complicated. I had to implement inheritance from scratch in C and write a simple garbage collector in pure C (see [gc.h](./gc.h)).

Please see my full [**report here**](./final-report.pdf), and the corresponding LaTeX files [here](./report). The project was well received and earned me `30/30` points on the assignment.

## Example program
The following is FizzBuzz implemented in this simple language. You can see more examples in [testprograms](./testprograms).

```Scala
class FizzBuzz {
  def compute(from: Int, to: Int): Int = {
    var i: Int = 0;
    println("Running FizzBuzz from " + from + " to " + to);
    i = from;
    while(i < to) {
      if (this.divides(i,15)) {
        println("FizzBuzz")
      } else if (this.divides(i,5)) {
        println("Fizz")
      } else if (this.divides(i,3)) {
        println("Buzz")
      } else {
        println(i)
      };
      i = i + 1
    };
    i
  }

  def divides(number: Int, divisor: Int): Boolean = {
    this.mod(number, divisor) == 0
  }

  def mod(m: Int, n: Int): Int = { m - (n * (m / n)) }
}

object Main extends App {
  println(new FizzBuzz().compute(0,100))
}
```
