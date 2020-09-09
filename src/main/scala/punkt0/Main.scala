package punkt0

import java.io.File
import lexer._

object Main {

  def processOptions(args: List[String], ctx: Context = Context()): Context = {
    args match {
      case "--help" :: args =>
        processOptions(args, ctx.copy(doHelp = true))
      case "--tokens" :: args =>
        processOptions(args, ctx.copy(doTokens = true))
      case "-d" :: out :: args =>
        processOptions(args, ctx.copy(outDir = Some(new File(out))))
      case f :: args =>
        processOptions(args, ctx.copy(file = Some(new File(f))))
      case List() => ctx
    }
  }

  def displayHelp(): Unit = {
    println("Usage: <punkt0c> [options] <file>")
    println("Options include:")
    println(" --help        displays this help")
    println(" --tokens      prints the tokens produced by the lexer")
    println(" -d <outdir>   generates class files in the specified directory")
  }

  def main(args: Array[String]): Unit = {
    val ctx = processOptions(args.toList)

    if (ctx.doHelp) return displayHelp()

    if (ctx.file.isEmpty) {
      println("Please provide a file!")
      sys.exit(1)
    }

    val f = ctx.file.get
    var tokens = Lexer.run(f)(ctx)
    if (ctx.doTokens) {
      tokens foreach println
      Reporter.terminateIfErrors()
      return
    }
    Reporter.terminateIfErrors()
  }

}
