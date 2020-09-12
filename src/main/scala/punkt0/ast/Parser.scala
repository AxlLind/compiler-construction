package punkt0
package ast

import Trees._
import lexer._

object Parser extends Phase[Iterator[Token], Program] {
  def run(tokens: Iterator[Token])(ctx: Context): Program = {
    var currentToken: Token = new Token(BAD)

    def readToken: Unit = do {
      currentToken = tokens.next()
    } while (currentToken.kind == BAD)

    def consume(kinds: TokenKind*): Unit = {
      kinds foreach { kind =>
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

    def parseMethodCall(expr: ExprTree): MethodCall = {
      consume(DOT)
      val method = parseIdentifier
      consume(LPAREN)
      val arguments = currentToken.kind != RPAREN match {
        case true  => parseExprList(COMMA)
        case false => List[ExprTree]()
      }
      consume(RPAREN)
      MethodCall(expr, method, arguments)
    }

    def parseType: TypeTree = currentToken.kind match {
      case BOOLEAN =>
        consume(BOOLEAN)
        BooleanType()
      case INT =>
        consume(INT)
        IntType()
      case STRING =>
        consume(STRING)
        StringType()
      case UNIT =>
        consume(UNIT)
        UnitType()
      case _ => parseIdentifier
    }

    def parseOperatorExpr(expr: ExprTree): ExprTree = {
      val newExpr = currentToken.kind match {
        case AND =>
          consume(AND)
          And(expr, parseExprInner)
        case OR =>
          consume(OR)
          Or(expr, parseExprInner)
        case EQUALS =>
          consume(EQUALS)
          Equals(expr, parseExprInner)
        case LESSTHAN =>
          consume(LESSTHAN)
          LessThan(expr, parseExprInner)
        case PLUS =>
          consume(PLUS)
          Plus(expr, parseExprInner)
        case MINUS =>
          consume(MINUS)
          Minus(expr, parseExprInner)
        case TIMES =>
          consume(TIMES)
          Times(expr, parseExprInner)
        case DIV =>
          consume(DIV)
          Div(expr, parseExprInner)
        case _ => return expr
      }
      parseOperatorExpr(newExpr)
    }

    def parseExprInner: ExprTree = {
      val expr = currentToken.kind match {
        case TRUE =>
          consume(TRUE)
          True()
        case FALSE =>
          consume(FALSE)
          False()
        case THIS =>
          consume(THIS)
          This()
        case NULL =>
          consume(NULL)
          Null()
        case BANG =>
          consume(BANG)
          Not(parseExpr)
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
        case INTLITKIND => parseIntLiteral
        case STRLITKIND => parseStrLiteral
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

      currentToken.kind == DOT match {
        case true => parseMethodCall(expr)
        case false => expr
      }
    }

    def parseExpr: ExprTree = parseOperatorExpr(parseExprInner)

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

      MethodDecl(overrides, retType, id, args, vars.reverse, exprs.reverse, retExpr)
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

      ClassDecl(id, parent, vars, methods.reverse)
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
