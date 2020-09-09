package punkt0
package lexer

import java.io.File

object Lexer extends Phase[File, Iterator[Token]] {
  import Reporter._

  def run(f: File)(ctx: Context): Iterator[Token] = {
    val source = scala.io.Source.fromFile(f).mkString
    var ptr = 0
    var hasEmittedEOF = false
    val wordRegex = "^\\w+".r
    val whitespace = "^\\s+".r

    def emitToken(tokenSize: Int, tokenType: TokenKind): Token = {
      val line = source.slice(0, ptr).count(_ == '\n') + 1
      val col = ptr - source.slice(0, ptr).lastIndexOf('\n')
      ptr += tokenSize
      new Token(tokenType).setPos(f, line, col)
    }

    def consumeWhitespaceAndComments: Option[Token] = {
      var prevPtr = -1
      while (prevPtr != ptr) {
        prevPtr = ptr
        ptr += whitespace
          .findFirstIn(source.slice(ptr, source.length))
          .map(_.length)
          .getOrElse(0)

        if (source.slice(ptr, ptr + 2) == "//") {
          source.slice(ptr, source.length).indexOf('\n') match {
            case -1 => ptr = source.length
            case i => ptr += i + 1
          }
        }

        if (source.slice(ptr, ptr + 2) == "/*") {
          val endOfComment = source.slice(ptr + 2, source.length).indexOf("*/")
          if (endOfComment == -1)
            return Some(emitToken(2, BAD))
          ptr += endOfComment + 4
        }
      }
      None
    }

    new Iterator[Token] {
      def hasNext = !hasEmittedEOF

      def next(): Token = {
        val maybeToken = consumeWhitespaceAndComments
        if (maybeToken.isDefined) {
          return maybeToken.get
        }

        if (ptr >= source.length) {
          hasEmittedEOF = true
          return emitToken(0, EOF)
        }

        source.slice(ptr, source.length) match {
          case s":$_"  => emitToken(1, COLON)
          case s";$_"  => emitToken(1, SEMICOLON)
          case s".$_"  => emitToken(1, DOT)
          case s",$_"  => emitToken(1, COMMA)
          case s"==$_" => emitToken(2, EQUALS)
          case s"=$_"  => emitToken(1, EQSIGN)
          case s"!$_"  => emitToken(1, BANG)
          case s"($_"  => emitToken(1, LPAREN)
          case s")$_"  => emitToken(1, RPAREN)
          case s"{$_"  => emitToken(1, LBRACE)
          case s"}$_"  => emitToken(1, RBRACE)
          case s"&&$_" => emitToken(2, AND)
          case s"||$_" => emitToken(2, OR)
          case s"<$_"  => emitToken(1, LESSTHAN)
          case s"+$_"  => emitToken(1, PLUS)
          case s"-$_"  => emitToken(1, MINUS)
          case s"*$_"  => emitToken(1, TIMES)
          case s"/$_"  => emitToken(1, DIV)
          case s"""\"$rest""" =>
            rest.indexOf('"') match {
              case -1 => emitToken(1, BAD)
              case i =>
                val s = rest.slice(0,i-1)
                new STRLIT(s).setPos(emitToken(i+1, STRLITKIND))
            }
          case src => wordRegex.findFirstIn(src) match {
            case Some("object")   => emitToken(6, OBJECT)
            case Some("class")    => emitToken(5, CLASS)
            case Some("def")      => emitToken(3, DEF)
            case Some("override") => emitToken(8, OVERRIDE)
            case Some("var")      => emitToken(3, VAR)
            case Some("Unit")     => emitToken(4, UNIT)
            case Some("String")   => emitToken(6, STRING)
            case Some("extends")  => emitToken(7, EXTENDS)
            case Some("Int")      => emitToken(3, INT)
            case Some("Boolean")  => emitToken(7, BOOLEAN)
            case Some("while")    => emitToken(5, WHILE)
            case Some("if")       => emitToken(2, IF)
            case Some("else")     => emitToken(4, ELSE)
            case Some("true")     => emitToken(4, TRUE)
            case Some("false")    => emitToken(5, FALSE)
            case Some("this")     => emitToken(4, THIS)
            case Some("null")     => emitToken(4, NULL)
            case Some("new")      => emitToken(3, NEW)
            case Some("println")  => emitToken(7, PRINTLN)
            case Some(w) => {
              if (w(0).isLetter)
                return new ID(w).setPos(emitToken(w.length, IDKIND))
              if (w.forall(_.isDigit))
                return new INTLIT(w.toInt).setPos(emitToken(w.length, IDKIND))
              emitToken(w.length, BAD)
            }
            case None => emitToken(1, BAD)
          }
        }
      }
    }
  }
}
