package punkt0
package lexer

import java.io.File

object Lexer extends Phase[File, Iterator[Token]] {
  val wordRegex = "^(?s)(\\w+).*".r
  val whitespace = "^\\s+".r

  def run(f: File)(ctx: Context): Iterator[Token] = {
    val source = scala.io.Source.fromFile(f).mkString
    var hasMore = true
    var ptr = 0

    def emitToken(tokenSize: Int, tokenType: TokenKind): Token = {
      val line = source.slice(0, ptr).count(_ == '\n') + 1
      val col = ptr - source.slice(0, ptr).lastIndexOf('\n')
      ptr += tokenSize
      new Token(tokenType).setPos(f, line, col)
    }

    def emitErr(msg: String, tokenSize: Int): Token = {
      val errToken = emitToken(tokenSize, BAD)
      Reporter.error(msg, errToken)
      errToken
    }

    def emitEOF: Token = {
      hasMore = false
      emitToken(0, EOF)
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
          source.indexOf('\n', ptr + 2) match {
            case -1 => ptr = source.length
            case i => ptr = i + 1
          }
        }

        if (source.slice(ptr, ptr + 2) == "/*") {
          source.indexOf("*/", ptr + 2) match {
            case -1 => return Some(emitErr("Unmatched block comment", 2))
            case i => ptr = i + 2
          }
        }
      }
      None
    }

    new Iterator[Token] {
      def hasNext = hasMore

      def next(): Token = {
        val maybeToken = consumeWhitespaceAndComments
        if (maybeToken.isDefined)
          return maybeToken.get
        if (ptr >= source.length)
          return emitEOF

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
          case wordRegex(w) => w match {
            case "if"       => emitToken(2, IF)
            case "object"   => emitToken(6, OBJECT)
            case "class"    => emitToken(5, CLASS)
            case "def"      => emitToken(3, DEF)
            case "var"      => emitToken(3, VAR)
            case "Unit"     => emitToken(4, UNIT)
            case "String"   => emitToken(6, STRING)
            case "extends"  => emitToken(7, EXTENDS)
            case "Int"      => emitToken(3, INT)
            case "Boolean"  => emitToken(7, BOOLEAN)
            case "while"    => emitToken(5, WHILE)
            case "else"     => emitToken(4, ELSE)
            case "true"     => emitToken(4, TRUE)
            case "false"    => emitToken(5, FALSE)
            case "this"     => emitToken(4, THIS)
            case "null"     => emitToken(4, NULL)
            case "new"      => emitToken(3, NEW)
            case "println"  => emitToken(7, PRINTLN)
            case "override" => emitToken(8, OVERRIDE)
            case w if w(0).isLetter => new ID(w).setPos(emitToken(w.length, IDKIND))
            case w if w.forall(_.isDigit) => new INTLIT(w.toInt).setPos(emitToken(w.length, IDKIND))
            case w => emitErr("Bad identifier", w.length)
          }
          case s"""\"$rest""" => rest.indexOf('"') match {
            case -1 => emitErr("Unmatched string literal", 1)
            case i => new STRLIT(rest.slice(0,i-1)).setPos(emitToken(i+1, STRLITKIND))
          }
          case _ => emitErr("Unknown token", 1)
        }
      }
    }
  }
}
