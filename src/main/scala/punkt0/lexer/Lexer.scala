package punkt0
package lexer

import java.io.File


object Lexer extends Phase[File, Iterator[Token]] {
  import Reporter._

  def run(f: File)(ctx: Context): Iterator[Token] = {
    val source = scala.io.Source.fromFile(f)

    // TODO: implement this method

    new Iterator[Token] {

      def hasNext = {
        ???
      }

      def next = {
        ???
      }
    }
  }
}
