package punkt0

import java.io.File
import lexer._
import ast._

object Main {
  def processOptions(args: List[String], ctx: Context = Context()): Context = {
    args match {
      case "--help" :: args => displayHelp()
      case "--tokens" :: args =>
        processOptions(args, ctx.copy(doTokens = true))
      case "--ast" :: args =>
        processOptions(args, ctx.copy(doAST = true))
      case "--print" :: args =>
        processOptions(args, ctx.copy(doPrintMain = true))
      case "--symbols" :: args =>
        processOptions(args, ctx.copy(doSymbolIds = true))
      case "--eval" :: args =>
        processOptions(args, ctx.copy(doEval = true))
      case "-d" :: out :: args =>
        processOptions(args, ctx.copy(outDir = Some(new File(out))))
      case f :: args =>
        processOptions(args, ctx.copy(file = Some(new File(f))))
      case List() => ctx
    }
  }

  def displayHelp(): Nothing = {
    println("Usage: <punkt0c> [options] <file>")
    println("Options include:")
    println(" --help        displays this message")
    println(" --tokens      prints the tokens produced by the lexer")
    println(" --ast         prints the program as an AST")
    println(" --print       prints the program source, derived from the AST")
    println(" -d <outdir>   generates class files in the specified directory")
    sys.exit(0)
  }

  def tokens(ctx: Context): Nothing = {
    val tokens = Lexer.run(ctx.file.get)(ctx)
    tokens foreach println
    Reporter.terminateIfErrors()
    sys.exit(0)
  }

  def ast(ctx: Context): Nothing = {
    val program = Lexer.andThen(Parser).run(ctx.file.get)(ctx)
    println(program)
    Reporter.terminateIfErrors()
    sys.exit(0)
  }

  def printMain(ctx: Context): Nothing = {
    val program = Lexer.andThen(Parser).run(ctx.file.get)(ctx)
    println(Printer.apply(program))
    Reporter.terminateIfErrors()
    sys.exit(0)
  }

  def eval(ctx: Context): Nothing = {
    println("Not yet implemented!")
    Reporter.terminateIfErrors()
    sys.exit(1)
  }

  def main(args: Array[String]): Unit = {
    val ctx = processOptions(args.toList)

    if (ctx.file.isEmpty) {
      println("Please provide a source file.")
      sys.exit(1)
    }

    if (ctx.doTokens) tokens(ctx)
    if (ctx.doAST) ast(ctx)
    if (ctx.doPrintMain) printMain(ctx)
    if (ctx.doEval) eval(ctx)

    println("Please provide an option")
    sys.exit(1)
  }

}
