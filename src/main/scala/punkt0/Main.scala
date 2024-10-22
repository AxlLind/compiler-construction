package punkt0

import java.io.File
import lexer._
import ast._
import analyzer._
import code._
import cbackend._

object Main {
  def processOptions(args: List[String], ctx: Context = Context()): Context = args match {
    case "--help"     :: args => displayHelp()
    case "--tokens"   :: args => processOptions(args, ctx.copy(doTokens = true))
    case "--ast"      :: args => processOptions(args, ctx.copy(doAST = true))
    case "--ast+"     :: args => processOptions(args, ctx.copy(doASTPlus = true))
    case "--print"    :: args => processOptions(args, ctx.copy(doPrintMain = true))
    case "--symid"    :: args => processOptions(args, ctx.copy(doSymbolIds = true))
    case "--cbackend" :: args => processOptions(args, ctx.copy(doCBackend = true))
    case "-d" :: out  :: args => processOptions(args, ctx.copy(outDir = Some(new File(out))))
    case f :: args => processOptions(args, ctx.copy(file = Some(new File(f))))
    case List() => ctx
  }

  def displayHelp(): Nothing = {
    println("Usage: <punkt0c> [options] <file>")
    println("Options include:")
    println(" --help        displays this message")
    println(" --tokens      prints the tokens produced by the lexer")
    println(" --ast         prints the program as an AST")
    println(" --ast+        prints the program as an AST, with type information")
    println(" --print       prints the program source, derived from the AST")
    println(" --symid       adds symbol information when printing the AST")
    println(" --cbackend    outputs a C program instead of JVM bytecode")
    println(" -d <outdir>   generates class files in the specified directory")
    sys.exit(0)
  }

  def tokens(f: File, ctx: Context): Unit = {
    val tokens = Lexer.run(f)(ctx)
    tokens foreach println
    Reporter.terminateIfErrors()
  }

  def ast(f: File, ctx: Context): Unit = {
    val runner = Lexer andThen Parser
    val program = runner.run(f)(ctx)
    println(program)
    Reporter.terminateIfErrors()
  }

  def astPlus(f: File, ctx: Context): Unit = {
    val runner = Lexer andThen Parser andThen NameAnalysis andThen TypeChecking
    val program = runner.run(f)(ctx)
    println(TypedASTPrinter.apply(program))
    Reporter.terminateIfErrors()
  }

  def printMain(f: File, ctx: Context): Unit = {
    var runner = Lexer andThen Parser
    if (ctx.doSymbolIds)
      runner = runner andThen NameAnalysis andThen TypeChecking
    val program = runner.run(f)(ctx)
    Reporter.terminateIfErrors()
    println(Printer.asString(program, ctx))
  }

  def runCBackend(f: File, ctx: Context): Unit = {
    val compiler = Lexer andThen Parser andThen NameAnalysis andThen TypeChecking andThen CBackend
    compiler.run(f)(ctx)
  }

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
    if (ctx.doASTPlus) return astPlus(f, ctx)
    if (ctx.doPrintMain) return printMain(f, ctx)
    if (ctx.doCBackend) return runCBackend(f, ctx)

    val compiler = Lexer andThen Parser andThen NameAnalysis andThen TypeChecking andThen CodeGeneration
    compiler.run(f)(ctx)
  }

}
