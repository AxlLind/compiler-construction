package punkt0
package ast

object Trees {
  sealed trait Tree extends Positioned

  case class Program(main: MainDecl, classes: List[ClassDecl]) extends Tree
  case class MainDecl(obj: Identifier, parent: Identifier, vars: List[VarDecl], exprs: List[ExprTree]) extends Tree
  case class ClassDecl(id: Identifier, parent: Option[Identifier], vars: List[VarDecl], methods: List[MethodDecl]) extends Tree
  case class VarDecl(tpe: TypeTree, id: Identifier, expr: ExprTree) extends Tree
  case class MethodDecl(overrides: Boolean, retType: TypeTree, id: Identifier, args: List[Formal], vars: List[VarDecl], exprs: List[ExprTree], retExpr: ExprTree) extends Tree {
  }
  sealed case class Formal(tpe: TypeTree, id: Identifier) extends Tree

  sealed trait TypeTree extends Tree
  case class BooleanType() extends TypeTree
  case class IntType() extends TypeTree
  case class StringType() extends TypeTree
  case class UnitType() extends TypeTree

  sealed trait ExprTree extends Tree
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
  case class Identifier(value: String) extends TypeTree with ExprTree
  case class This() extends ExprTree
  case class Null() extends ExprTree
  case class New(tpe: Identifier) extends ExprTree
  case class Not(expr: ExprTree) extends ExprTree

  case class Block(exprs: List[ExprTree]) extends ExprTree
  case class If(expr: ExprTree, thn: ExprTree, els: Option[ExprTree]) extends ExprTree
  case class While(cond: ExprTree, body: ExprTree) extends ExprTree
  case class Println(expr: ExprTree) extends ExprTree
  case class Assign(id: Identifier, expr: ExprTree) extends ExprTree
}
