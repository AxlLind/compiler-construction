package punkt0
package ast

import analyzer.Symbols._
import analyzer.Types._

object Trees {
  sealed trait Tree extends Positioned

  case class Program(main: MainDecl, classes: List[ClassDecl]) extends Tree
  case class MainDecl(obj: Identifier, parent: Identifier, vars: List[VarDecl], exprs: List[ExprTree]) extends Tree with Symbolic[ClassSymbol]
  case class ClassDecl(id: Identifier, parent: Option[Identifier], vars: List[VarDecl], methods: List[MethodDecl]) extends Tree with Symbolic[ClassSymbol]
  case class VarDecl(tpe: TypeTree, id: Identifier, expr: ExprTree) extends Tree with Symbolic[VariableSymbol]
  case class MethodDecl(overrides: Boolean, retType: TypeTree, id: Identifier, args: List[Formal], vars: List[VarDecl], exprs: List[ExprTree], retExpr: ExprTree) extends Tree with Symbolic[MethodSymbol]
  sealed case class Formal(tpe: TypeTree, id: Identifier) extends Tree with Symbolic[VariableSymbol]

  sealed trait TypeTree extends Tree with Typed
  case class BooleanType() extends TypeTree
  case class IntType() extends TypeTree
  case class StringType() extends TypeTree
  case class UnitType() extends TypeTree

  sealed trait ExprTree extends Tree with Typed
  case class And(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Or(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Plus(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Minus(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Times(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Div(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class LessThan(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Equals(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) extends ExprTree
  case class IntLit(value: Int) extends ExprTree
  case class StringLit(value: String) extends ExprTree

  case class True() extends ExprTree
  case class False() extends ExprTree
  case class This() extends ExprTree with Symbolic[ClassSymbol]
  case class Null() extends ExprTree
  case class New(tpe: Identifier) extends ExprTree
  case class Not(expr: ExprTree) extends ExprTree
  case class Identifier(value: String) extends TypeTree with ExprTree with Symbolic[Symbol] {
    override def getType: Type = getSymbol match {
      case cs: ClassSymbol =>
        TAnyRef(cs)

      case ms: MethodSymbol =>
        sys.error("Requesting type of a method identifier.")

      case vs: VariableSymbol =>
        vs.getType
    }
    override def setType(tpe: Type) = this
  }

  case class Block(exprs: List[ExprTree]) extends ExprTree
  case class If(expr: ExprTree, thn: ExprTree, els: Option[ExprTree]) extends ExprTree
  case class While(cond: ExprTree, body: ExprTree) extends ExprTree
  case class Println(expr: ExprTree) extends ExprTree
  case class Assign(id: Identifier, expr: ExprTree) extends ExprTree
}
