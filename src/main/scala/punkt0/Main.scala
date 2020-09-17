package punkt0

import java.io.File
import lexer._
import ast._
import analyzer._

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
      case "--symid" :: args =>
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
    println(" --symid       adds symbol information when printing the AST")
    println(" -d <outdir>   generates class files in the specified directory")
    sys.exit(0)
  }

  def tokens(f: File, ctx: Context): Unit = {
    val tokens = Lexer.run(f)(ctx)
    tokens foreach println
    Reporter.terminateIfErrors()
  }

  def ast(f: File, ctx: Context): Unit = {
    val program = Lexer.andThen(Parser).run(f)(ctx)
    println(program)
    Reporter.terminateIfErrors()
  }

  def printMain(f: File, ctx: Context): Unit = {
    val program = Lexer.andThen(Parser).andThen(NameAnalysis).run(f)(ctx)
    println(Printer.asString(program, ctx))
    Reporter.terminateIfErrors()
  }

  def eval(f: File, ctx: Context): Unit = ???

  def main(args: Array[String]): Unit = {
    val ctx = processOptions(args.toList)

    val f = ctx.file match {
      case Some(f) if f.isFile => f
      case Some(f) =>
        println(s"Provided file '${f.getName}' not found.")
        sys.exit(1)
      case None =>
        println("Please provide a source file.")
        sys.exit(1)
    }

    if (ctx.doTokens) return tokens(f, ctx)
    if (ctx.doAST) return ast(f, ctx)
    if (ctx.doPrintMain) return printMain(f, ctx)
    if (ctx.doEval) return eval(f, ctx)

    println("Please provide an option")
    sys.exit(1)
  }

}
