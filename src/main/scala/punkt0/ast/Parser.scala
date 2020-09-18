package punkt0
package ast

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

    def parseIdentifier: Identifier = {
      val idToken = expectedToken(IDKIND).asInstanceOf[ID]
      Identifier(idToken.value).setPos(idToken)
    }

    def consumeAndRet[T <: Tree](kind: TokenKind, t: T): T = {
      val startToken = currentToken
      consume(kind)
      t.setPos(currentToken)
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
      expr.setPos(lhs)
      parseOperatorExpr(expr, precedence).setPos(lhs)
    }

    def parsePrimaryExpr: ExprTree = {
      val startToken = currentToken
      var expr = currentToken.kind match {
        case TRUE  => consumeAndRet(TRUE, True())
        case FALSE => consumeAndRet(FALSE, False())
        case THIS  => consumeAndRet(THIS, This())
        case NULL  => consumeAndRet(NULL, Null())
        case INTLITKIND => IntLit(expectedToken(INTLITKIND).asInstanceOf[INTLIT].value)
        case STRLITKIND => StringLit(expectedToken(STRLITKIND).asInstanceOf[STRLIT].value)
        case BANG  =>
          consume(BANG)
          Not(parsePrimaryExpr)
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
          val elseExpr = currentToken.kind == ELSE match {
            case true  =>
              consume(ELSE)
              Some(parseExpr)
            case false => None
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
        expr = parseMethodCall(expr.setPos(startToken))
      expr.setPos(startToken)
    }

    def parseExpr: ExprTree = parseOperatorExpr(parsePrimaryExpr)

    def parseVarDecl: VarDecl = {
      val startToken = currentToken
      consume(VAR)
      val id = parseIdentifier
      consume(COLON)
      val tpe = parseType
      consume(EQSIGN)
      val expr = parseExpr
      expr match {
        case IntLit(_) | StringLit(_) | True() | False() | Null() | New(_) => {}
        case _ => Reporter.fatal("Variable declaration has to be a literal or a new expression", expr)
      }
      consume(SEMICOLON)
      VarDecl(tpe, id, expr).setPos(startToken)
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
      Formal(tpe, id).setPos(id)
    }

    def parseMethodDecl: MethodDecl = {
      val startToken = currentToken
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

      MethodDecl(overrides, retType, id, args.reverse, vars.reverse, exprs.reverse, retExpr).setPos(startToken)
    }

    def parseMainDecl: MainDecl = {
      val startToken = currentToken
      consume(OBJECT)
      val id = parseIdentifier
      consume(EXTENDS)
      val parent = parseIdentifier

      consume(LBRACE)
      val vars = parseVarDeclList
      val exprs = parseExprList(SEMICOLON)
      consume(RBRACE)

      MainDecl(id, parent, vars.reverse, exprs.reverse).setPos(startToken)
    }

    def parseClassDecl: ClassDecl = {
      val startToken = currentToken
      consume(CLASS)
      val id = parseIdentifier
      val parent = currentToken.kind == EXTENDS match {
        case true =>
          consume(EXTENDS)
          Some(parseIdentifier)
        case false => None
      }
      consume(LBRACE)
      val vars = parseVarDeclList
      var methods = List[MethodDecl]()
      while (currentToken.kind == OVERRIDE || currentToken.kind == DEF)
        methods = parseMethodDecl :: methods

      consume(RBRACE)

      ClassDecl(id, parent, vars.reverse, methods.reverse).setPos(startToken)
    }

    def parseProgram: Program = {
      val startToken = currentToken
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
      Program(main.get, classes.reverse).setPos(startToken)
    }

    readToken
    val tree = parseProgram
    Reporter.terminateIfErrors()
    tree
  }
}
