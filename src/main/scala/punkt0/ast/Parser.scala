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

    def eat(kind: TokenKind): Unit = {
      if (currentToken.kind != kind)
        expected(kind)
      readToken
    }

    def expected(kind: TokenKind, more: TokenKind*): Nothing =
      Reporter.fatal("expected: " + (kind::more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)

    def expectedToken(kind: TokenKind): Token = {
      val token = currentToken
      eat(kind)
      token
    }

    def parseIdentifier: Identifier = Identifier(expectedToken(IDKIND).asInstanceOf[ID].value)
    def parseIntLiteral: IntLit = IntLit(expectedToken(INTLITKIND).asInstanceOf[INTLIT].value)
    def parseStrLiteral: StringLit = StringLit(expectedToken(STRLITKIND).asInstanceOf[STRLIT].value)

    def parseType: TypeTree = currentToken.kind match {
      case BOOLEAN =>
        eat(BOOLEAN)
        BooleanType()
      case INT =>
        eat(INT)
        IntType()
      case STRING =>
        eat(STRING)
        StringType()
      case UNIT =>
        eat(UNIT)
        UnitType()
      case _ => parseIdentifier
    }

    def parseExprLookAhead(expr: ExprTree): ExprTree = {
      val newExpr = currentToken.kind match {
        case DOT =>
          eat(DOT)
          val method = parseIdentifier
          eat(LPAREN)
          val arguments =
            if (currentToken.kind != RPAREN)
              parseExprList(COMMA)
            else
              List[ExprTree]()
          eat(RPAREN)
          MethodCall(expr, method, arguments)
        case AND =>
          eat(AND)
          val rhs = parseExpr
          And(expr, rhs)
        case OR =>
          eat(OR)
          val rhs = parseExpr
          Or(expr, rhs)
        case EQUALS =>
          eat(EQUALS)
          val rhs = parseExpr
          Equals(expr, rhs)
        case LESSTHAN =>
          eat(LESSTHAN)
          val rhs = parseExpr
          LessThan(expr, rhs)
        case PLUS =>
          eat(PLUS)
          val rhs = parseExpr
          Plus(expr, rhs)
        case MINUS =>
          eat(MINUS)
          val rhs = parseExpr
          Minus(expr, rhs)
        case TIMES =>
          eat(TIMES)
          val rhs = parseExpr
          Times(expr, rhs)
        case DIV =>
          eat(DIV)
          val rhs = parseExpr
          Div(expr, rhs)
        case _ => return expr
      }
      parseExprLookAhead(newExpr)
    }

    def parseExpr: ExprTree = {
      val expr = currentToken.kind match {
        case TRUE =>
          eat(TRUE)
          True()
        case FALSE =>
          eat(FALSE)
          False()
        case THIS =>
          eat(THIS)
          This()
        case NULL =>
          eat(NULL)
          Null()
        case BANG =>
          eat(BANG)
          Not(parseExpr)
        case NEW =>
          eat(NEW)
          val id = parseIdentifier
          eat(LPAREN)
          eat(RPAREN)
          New(id)
        case IDKIND =>
          val id = parseIdentifier
          if (currentToken.kind == EQSIGN) {
            eat(EQSIGN)
            Assign(id, parseExpr)
          } else {
            id
          }
        case INTLITKIND => parseIntLiteral
        case STRLITKIND => parseStrLiteral
        case LPAREN =>
          eat(LPAREN)
          val expr = parseExpr
          eat(RPAREN)
          expr
        case LBRACE =>
          eat(LBRACE)
          val exprs =
            if (currentToken.kind != RBRACE)
              parseExprList(SEMICOLON)
            else
              List[ExprTree]()
          eat(RBRACE)
          Block(exprs.reverse)
        case IF =>
          eat(IF)
          eat(LPAREN)
          val expr = parseExpr
          eat(RPAREN)
          val thenExpr = parseExpr
          var elseExpr = None: Option[ExprTree]
          if (currentToken.kind == ELSE) {
            eat(ELSE)
            elseExpr = Some(parseExpr)
          }
          If(expr, thenExpr, elseExpr)
        case WHILE =>
          eat(WHILE)
          eat(LPAREN)
          val expr = parseExpr
          eat(RPAREN)
          val bodyExpr = parseExpr
          While(expr, bodyExpr)
        case PRINTLN =>
          eat(PRINTLN)
          eat(LPAREN)
          val expr = parseExpr
          eat(RPAREN)
          Println(expr)
        case _ => expected(BAD) // FIXME
      }
      parseExprLookAhead(expr)
    }

    def parseVarDecl: VarDecl = {
      eat(VAR)
      val id = parseIdentifier
      eat(COLON)
      val tpe = parseType
      eat(EQSIGN)
      val expr = parseExpr
      eat(SEMICOLON)
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
        eat(separator)
        exprs = parseExpr :: exprs
      }
      exprs
    }

    def parseFormal: Formal = {
      val id = parseIdentifier
      eat(COLON)
      val tpe = parseType
      Formal(tpe, id)
    }

    def parseMethodDecl: MethodDecl = {
      val overrides = currentToken.kind == OVERRIDE
      if (overrides)
        eat(OVERRIDE)
      eat(DEF)
      val id = parseIdentifier

      eat(LPAREN)
      var args = List[Formal]()
      if (currentToken.kind != RPAREN) {
        args = parseFormal :: args
        while (currentToken.kind == COMMA) {
          eat(COMMA)
          args = parseFormal :: args
        }
      }
      eat(RPAREN)

      eat(COLON)
      val retType = parseType
      eat(EQSIGN)

      eat(LBRACE)
      val vars = parseVarDeclList
      val retExpr::exprs = parseExprList(SEMICOLON)
      eat(RBRACE)

      MethodDecl(overrides, retType, id, args, vars.reverse, exprs.reverse, retExpr)
    }

    def parseMainDecl: MainDecl = {
      eat(OBJECT)
      val id = parseIdentifier
      eat(EXTENDS)
      val parent = parseIdentifier

      eat(LBRACE)
      val vars = parseVarDeclList
      var exprs = parseExprList(SEMICOLON)
      eat(RBRACE)

      MainDecl(id, parent, vars.reverse, exprs.reverse)
    }

    def parseClassDecl: ClassDecl = {
      eat(CLASS)
      val id = parseIdentifier
      var parent = None: Option[Identifier]

      if (currentToken.kind == EXTENDS) {
        eat(EXTENDS)
        parent = Some(parseIdentifier)
      }

      eat(LBRACE)
      val vars = parseVarDeclList
      var methods = List[MethodDecl]()
      while (currentToken.kind == OVERRIDE || currentToken.kind == DEF)
        methods = parseMethodDecl :: methods

      eat(RBRACE)

      ClassDecl(id, parent, vars, methods.reverse)
    }

    def parseGoal: Program = {
      var classes = List[ClassDecl]()
      var main = None: Option[MainDecl]
      while (tokens.hasNext) {
        currentToken.kind match {
          case CLASS  => classes = parseClassDecl :: classes
          case OBJECT =>
            main = Some(parseMainDecl)
            readToken
            eat(EOF)
          case _ => expected(CLASS, OBJECT)
        }
      }
      if (main.isEmpty)
        Reporter.fatal("No main declaration")
      Program(main.get, classes.reverse)
    }

    readToken
    val tree = parseGoal
    Reporter.terminateIfErrors()
    tree
  }
}
