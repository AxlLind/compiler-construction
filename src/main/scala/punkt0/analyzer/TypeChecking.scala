package punkt0
package analyzer

import ast.Trees._

import Symbols._
import Types._

object TypeChecking extends Phase[Program, Program] {

  def run(prog: Program)(ctx: Context): Program = {
    def typeLCA(t1: Type, t2: Type): Option[TAnyRef] = {
      def typePath(t: TAnyRef, path: List[TAnyRef] = List()): List[TAnyRef] =
        t.classSymbol.parent.map(parent => typePath(TAnyRef(parent), t :: path)) getOrElse t::path

      val path1 = typePath(t1.asInstanceOf[TAnyRef])
      val path2 = typePath(t2.asInstanceOf[TAnyRef])

      path1.zip(path2).takeWhile({ case (a,b) => a == b }).lastOption.map(_._1)
    }

    def typeError(msg: String, expr: ExprTree): Type = {
      Reporter.error(s"Type error. $msg", expr)
      TError
    }

    def tcExpr(expr: ExprTree, expected: Type*): Type = {
      val tpe: Type = expr match {
        case t: IntLit => TInt
        case t: StringLit => TString
        case t: True => TBoolean
        case t: False => TBoolean
        case t: Null => TNull
        case t: Identifier => t.getType
        case t: This => t.getSymbol.getType
        case t: New => t.tpe.getType
        case t: Not => tcExpr(t.expr, TBoolean)
        case t: Minus =>
          tcExpr(t.lhs, TInt)
          tcExpr(t.rhs, TInt)
        case t: Times =>
          tcExpr(t.lhs, TInt)
          tcExpr(t.rhs, TInt)
        case t: Div =>
          tcExpr(t.lhs, TInt)
          tcExpr(t.rhs, TInt)
        case t: LessThan =>
          tcExpr(t.lhs, TInt)
          tcExpr(t.rhs, TInt)
          TBoolean
        case t: And =>
          tcExpr(t.lhs, TBoolean)
          tcExpr(t.rhs, TBoolean)
        case t: Or =>
          tcExpr(t.lhs, TBoolean)
          tcExpr(t.rhs, TBoolean)
        case t: Equals => (tcExpr(t.lhs), tcExpr(t.rhs)) match {
          case (TAnyRef(_), TAnyRef(_)) => TBoolean
          case (t1,t2) if t1.isSubTypeOf(t2) => TBoolean
          case (t1,t2) if t2.isSubTypeOf(t1) => TBoolean
          case (t1,t2) => typeError(s"The equals-operator is not defined for $t1 == $t2.", expr)
        }
        case t: Plus => (tcExpr(t.lhs), tcExpr(t.rhs)) match {
          case (TInt, TInt) => TInt
          case (TInt, TString)
             | (TString, TInt)
             | (TString, TString) => TString
          case (t1,t2) => typeError(s"The plus-operator is not defined for $t1 + $t2.", expr)
        }
        case t: While =>
          tcExpr(t.cond, TBoolean)
          tcExpr(t.body, TUnit)
        case t: Println =>
          tcExpr(t.expr, TString, TInt, TBoolean)
          TUnit
        case t: Assign =>
          tcExpr(t.expr, t.id.getType)
          TUnit
        case t: Block =>
          t.exprs foreach { tcExpr(_) }
          t.exprs.lastOption.map(_.getType) getOrElse TUnit
        case t: MethodCall => tcExpr(t.obj) match {
          case TAnyRef(c) => c.lookupMethod(t.meth.value) match {
            case Some(m) =>
              t.meth.setSymbol(m)
              if (t.args.length != m.argList.length)
                Reporter.error("Incorrect number of function arguments.", expr)
              t.args.zip(m.argList) foreach { case (passedArg, arg) => tcExpr(passedArg, arg.getType) }
              m.retType
            case None => typeError(s"Class '${c.name}' has no method '${t.meth.value}'.", expr)
          }
          case _ => typeError("Method called on a expression that is not a class.", expr)
        }
        case t: If =>
          tcExpr(t.expr, TBoolean)
          t.els match {
            case Some(elseExpr) =>
              val t1 = tcExpr(t.thn)
              val t2 = tcExpr(elseExpr)
              (t1,t2) match {
                case (t1,t2) if t1.isSubTypeOf(t2) => t2
                case (t1,t2) if t2.isSubTypeOf(t1) => t1
                case (TAnyRef(_), TAnyRef(_)) => typeLCA(t1,t2) getOrElse typeError("Arms of if-statements do not type match.", expr)
                case _ => typeError("Arms of if-statement do not type match", expr)
              }
            case None => tcExpr(t.thn, TUnit)
          }
      }

      expr.setType(tpe)

      if (expected.nonEmpty && !expected.exists(tpe.isSubTypeOf)) {
        typeError(s"Expected ${expected.toList.mkString(" or ")}, found $tpe.", expr)
        return expected.head
      }

      tpe
    }

    def tcVarDecl(v: VarDecl) = {
      v.getSymbol.setType(v.tpe.getType)
      tcExpr(v.expr, v.getSymbol.getType)
    }

    // link the return type of each method to it's symbol, so we can find it later
    prog.classes.flatMap(_.methods) foreach { m => m.getSymbol.retType = m.retType.getType }

    // set the type of all method arguments
    prog.classes.flatMap(_.methods).flatMap(_.args) foreach { arg => arg.getSymbol.setType(arg.tpe.getType) }

    // validate signature against overridden method
    prog.classes.flatMap(_.methods.filter(_.overrides)) foreach { m =>
      m.args.zip(m.getSymbol.overridden.get.argList) foreach { case (arg, parentArg) =>
        if (!arg.tpe.getType.isSubTypeOf(parentArg.getType))
          Reporter.error("Function argument does not type match with the overridden method.", arg);
      }
    }

    // set the type of all fields
    prog.classes.flatMap(_.vars) foreach tcVarDecl

    // type check all methods
    prog.classes.flatMap(_.methods) foreach { m =>
      m.vars foreach tcVarDecl
      m.exprs foreach { tcExpr(_) }
      tcExpr(m.retExpr, m.retType.getType)
    }

    // lastly, type check main and it's variables
    prog.main.vars foreach tcVarDecl
    prog.main.exprs foreach { tcExpr(_) }

    prog
  }

}
