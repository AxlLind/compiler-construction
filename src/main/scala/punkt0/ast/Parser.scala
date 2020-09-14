package punkt0
package ast

import scala.collection.mutable.Queue
import Trees._
import lexer._

object Parser extends Phase[Iterator[Token], Program] {
  val opPrecedence = Map(
    TIMES    -> 6,
    DIV      -> 6,
    PLUS     -> 5,
    MINUS    -> 5,
    LESSTHAN -> 4,
    EQUALS   -> 3,
    AND      -> 2,
    OR       -> 1,
  )

  def run(tokens: Iterator[Token])(ctx: Context): Program = {
    var currentToken: Token = new Token(BAD)

    def readToken: Unit = do {
      currentToken = tokens.next()
    } while (currentToken.kind == BAD)

    def consume(kind: TokenKind, more: TokenKind*): Unit = {
      (kind::more.toList) foreach { kind =>
        if (currentToken.kind != kind)
          expected(kind)
        readToken
      }
    }

    def expected(kind: TokenKind, more: TokenKind*): Nothing =
      Reporter.fatal(s"expected: ${(kind::more.toList).mkString(" or ")}, found $currentToken", currentToken)

    def expectedToken(kind: TokenKind): Token = {
      val token = currentToken
      consume(kind)
      token
    }

    def parseIdentifier: Identifier = Identifier(expectedToken(IDKIND).asInstanceOf[ID].value)
    def parseIntLiteral: IntLit = IntLit(expectedToken(INTLITKIND).asInstanceOf[INTLIT].value)
    def parseStrLiteral: StringLit = StringLit(expectedToken(STRLITKIND).asInstanceOf[STRLIT].value)

    def consumeAndRet[T](kind: TokenKind, t: T): T = {
      consume(kind)
      t
    }

    def parseMethodCall(expr: ExprTree): MethodCall = {
      consume(DOT)
      val method = parseIdentifier
      consume(LPAREN)
      val arguments = currentToken.kind != RPAREN match {
        case true  => parseExprList(COMMA)
        case false => List[ExprTree]()
      }
      consume(RPAREN)
      MethodCall(expr, method, arguments.reverse)
    }

    def parseType: TypeTree = currentToken.kind match {
      case BOOLEAN => consumeAndRet(BOOLEAN, BooleanType())
      case INT     => consumeAndRet(INT, IntType())
      case STRING  => consumeAndRet(STRING, StringType())
      case UNIT    => consumeAndRet(UNIT, UnitType())
      case _ => parseIdentifier
    }

    def parseOperatorExpr(lhs: ExprTree, precedence: Int = 0): ExprTree = {
      // Algorithm from https://en.wikipedia.org/wiki/Operator-precedence_parser#Pseudocode
      val op = currentToken.kind
      if (!opPrecedence.contains(op) || opPrecedence(op) < precedence)
        return lhs

      consume(op)
      var rhs = parsePrimaryExpr
      while (opPrecedence.get(currentToken.kind).map(_ > opPrecedence(op)).getOrElse(false))
        rhs = parseOperatorExpr(rhs, opPrecedence(currentToken.kind))
      val expr = op match {
        case AND      => And(lhs, rhs)
        case OR       => Or(lhs, rhs)
        case EQUALS   => Equals(lhs, rhs)
        case LESSTHAN => LessThan(lhs, rhs)
        case PLUS     => Plus(lhs, rhs)
        case MINUS    => Minus(lhs, rhs)
        case TIMES    => Times(lhs, rhs)
        case DIV      => Div(lhs, rhs)
        case _ => expected(BAD) // impossible
      }
      parseOperatorExpr(expr, precedence)
    }

    def parsePrimaryExpr: ExprTree = {
      var expr = currentToken.kind match {
        case TRUE  => consumeAndRet(TRUE, True())
        case FALSE => consumeAndRet(FALSE, False())
        case THIS  => consumeAndRet(THIS, This())
        case NULL  => consumeAndRet(NULL, Null())
        case BANG  => consumeAndRet(BANG, Not(parsePrimaryExpr))
        case INTLITKIND => parseIntLiteral
        case STRLITKIND => parseStrLiteral
        case NEW =>
          consume(NEW)
          val id = parseIdentifier
          consume(LPAREN, RPAREN)
          New(id)
        case IDKIND =>
          val id = parseIdentifier
          currentToken.kind == EQSIGN match {
            case true =>
              consume(EQSIGN)
              Assign(id, parseExpr)
            case false => id
          }
        case LPAREN =>
          consume(LPAREN)
          val expr = parseExpr
          consume(RPAREN)
          expr
        case LBRACE =>
          consume(LBRACE)
          val exprs = currentToken.kind != RBRACE match {
            case true  => parseExprList(SEMICOLON)
            case false => List[ExprTree]()
          }
          consume(RBRACE)
          Block(exprs.reverse)
        case IF =>
          consume(IF, LPAREN)
          val expr = parseExpr
          consume(RPAREN)
          val thenExpr = parseExpr
          var elseExpr = None: Option[ExprTree]
          if (currentToken.kind == ELSE) {
            consume(ELSE)
            elseExpr = Some(parseExpr)
          }
          If(expr, thenExpr, elseExpr)
        case WHILE =>
          consume(WHILE, LPAREN)
          val expr = parseExpr
          consume(RPAREN)
          val bodyExpr = parseExpr
          While(expr, bodyExpr)
        case PRINTLN =>
          consume(PRINTLN, LPAREN)
          val expr = parseExpr
          consume(RPAREN)
          Println(expr)
        case _ => expected(BAD) // FIXME
      }

      while (currentToken.kind == DOT)
        expr = parseMethodCall(expr)
      expr
    }

    def parseExpr: ExprTree = parseOperatorExpr(parsePrimaryExpr)

    def parseVarDecl: VarDecl = {
      consume(VAR)
      val id = parseIdentifier
      consume(COLON)
      val tpe = parseType
      consume(EQSIGN)
      val expr = parseExpr
      consume(SEMICOLON)
      VarDecl(tpe, id, expr)
    }

    def parseVarDeclList: List[VarDecl] = {
      var vars = List[VarDecl]()
      while (currentToken.kind == VAR)
        vars = parseVarDecl :: vars
      vars
    }

    def parseExprList(separator: TokenKind): List[ExprTree] = {
      var exprs = List[ExprTree](parseExpr)
      while (currentToken.kind == separator) {
        consume(separator)
        exprs = parseExpr :: exprs
      }
      exprs
    }

    def parseFormal: Formal = {
      val id = parseIdentifier
      consume(COLON)
      val tpe = parseType
      Formal(tpe, id)
    }

    def parseMethodDecl: MethodDecl = {
      val overrides = currentToken.kind == OVERRIDE
      if (overrides)
        consume(OVERRIDE)
      consume(DEF)
      val id = parseIdentifier

      consume(LPAREN)
      var args = List[Formal]()
      if (currentToken.kind != RPAREN) {
        args = parseFormal :: args
        while (currentToken.kind == COMMA) {
          consume(COMMA)
          args = parseFormal :: args
        }
      }
      consume(RPAREN, COLON)
      val retType = parseType
      consume(EQSIGN, LBRACE)
      val vars = parseVarDeclList
      val retExpr::exprs = parseExprList(SEMICOLON)
      consume(RBRACE)

      MethodDecl(overrides, retType, id, args.reverse, vars.reverse, exprs.reverse, retExpr)
    }

    def parseMainDecl: MainDecl = {
      consume(OBJECT)
      val id = parseIdentifier
      consume(EXTENDS)
      val parent = parseIdentifier

      consume(LBRACE)
      val vars = parseVarDeclList
      var exprs = parseExprList(SEMICOLON)
      consume(RBRACE)

      MainDecl(id, parent, vars.reverse, exprs.reverse)
    }

    def parseClassDecl: ClassDecl = {
      consume(CLASS)
      val id = parseIdentifier
      var parent = None: Option[Identifier]

      if (currentToken.kind == EXTENDS) {
        consume(EXTENDS)
        parent = Some(parseIdentifier)
      }

      consume(LBRACE)
      val vars = parseVarDeclList
      var methods = List[MethodDecl]()
      while (currentToken.kind == OVERRIDE || currentToken.kind == DEF)
        methods = parseMethodDecl :: methods

      consume(RBRACE)

      ClassDecl(id, parent, vars.reverse, methods.reverse)
    }

    def parseProgram: Program = {
      var classes = List[ClassDecl]()
      var main = None: Option[MainDecl]
      while (tokens.hasNext) {
        currentToken.kind match {
          case CLASS  => classes = parseClassDecl :: classes
          case OBJECT =>
            main = Some(parseMainDecl)
            consume(EOF)
          case _ => expected(CLASS, OBJECT)
        }
      }
      if (main.isEmpty)
        Reporter.fatal("No main declaration")
      Program(main.get, classes.reverse)
    }

    readToken
    val tree = parseProgram
    Reporter.terminateIfErrors()
    tree
  }
}
