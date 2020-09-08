package punkt0

import java.io.File
import scala.io.Source

object Reporter {

  var errors = false

  def info(msg: Any, pos: Positioned = NoPosition): Unit =
    report(pos, "info", msg.toString)

  def warning(msg: Any, pos: Positioned = NoPosition): Unit = {
    report(pos, "warning", msg.toString)
  }

  def error(msg: Any, pos: Positioned = NoPosition): Unit = {
    errors = true
    report(pos, "error", msg.toString)
  }

  def fatal(msg: Any, pos: Positioned = NoPosition): Nothing = {
    report(pos, "fatal", msg.toString)
    sys.exit(1)
  }

  private def err(msg: String): Unit = {
    Console.err.println(msg)
  }

  def terminateIfErrors(): Unit = if (errors) {
    err("Errors reported.")
    sys.exit(1)
  }

  private def report(pos: Positioned, prefix: String, msg: String): Unit =
    if (pos.hasPos) {
      err(s"${pos.posString}: $prefix: $msg")
      val lines = linesIn(pos.file)
      if (pos.line <= lines.size) {
        err(lines(pos.line - 1))
        err(" " * pos.column + "^")
      } else {
        err("line not in source file")
      }
    } else {
      err(s"$prefix: $msg")
    }

  private def linesIn(f: File): IndexedSeq[String] = {
    val source = Source.fromFile(f).withPositioning(true)
    val lines = source.getLines().toIndexedSeq
    source.close()
    lines
  }

}
