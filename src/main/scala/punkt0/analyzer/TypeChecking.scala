package punkt0
package analyzer

import ast.Trees._

import Symbols._
import Types._

object TypeChecking extends Phase[Program, Program] {

  def run(prog: Program)(ctx: Context): Program = {
    def tcExpr(expr: ExprTree, expected: Type*): Type = {
      val tpe: Type = expr match {
        case t: IntLit => TInt
        case t: StringLit => TString
        case t: True => TBoolean
        case t: False => TBoolean
        case t: Null => TNull
        case t: Minus =>
          tcExpr(t.lhs, TInt)
          tcExpr(t.rhs, TInt)
        case t: Times =>
          tcExpr(t.lhs, TInt)
          tcExpr(t.rhs, TInt)
        case t: Div =>
          tcExpr(t.lhs, TInt)
          tcExpr(t.rhs, TInt)
        case t: And =>
          tcExpr(t.lhs, TBoolean)
          tcExpr(t.rhs, TBoolean)
        case t: Or =>
          tcExpr(t.lhs, TBoolean)
          tcExpr(t.rhs, TBoolean)
        case t: LessThan =>
          tcExpr(t.lhs, TInt)
          tcExpr(t.rhs, TInt)
          TBoolean
        case t: Not => tcExpr(t.expr, TBoolean)
        case t: This => t.getSymbol.getType
        case t: New => t.tpe.getType
        case t: Equals =>
          val t1 = tcExpr(t.lhs)
          val t2 = tcExpr(t.rhs)
          (t1,t2) match {
            case (TAnyRef(_), TAnyRef(_)) => TBoolean
            case _ if t1 == t2 => TBoolean
            case _ => TError
          }
        case t: Plus =>
          val t1 = tcExpr(t.lhs)
          val t2 = tcExpr(t.rhs)
          (t1,t2) match {
            case (TInt, TInt) => TInt
            case (TString, TInt) => TString
            case (TInt, TString) => TString
            case (TString, TString) => TString
            case _ => TError
          }
        case t: Block =>
          t.exprs.foreach(tcExpr(_))
          t.exprs.last.getType
        case t: MethodCall => tcExpr(t.obj) match {
          case TAnyRef(c) => c.lookupMethod(t.meth.value) match {
            case Some(m) =>
              t.meth.setSymbol(m)
              if (t.args.length != m.argList.length)
                Reporter.error(s"Incorrect number of function arguments given to '${t.meth.value}'.", expr)
              t.args.zip(m.argList) foreach { case (passedArg, arg) => tcExpr(passedArg, arg.getType) }
              m.retType
            case None =>
              Reporter.error(s"Class '${c.name}' has no method '${t.meth.value}'.", expr)
              TError
          }
          case _ =>
            Reporter.error(s"Method '${t.meth.value}' called on a expression that is not a class.", expr)
            TError
        }
        case t: If =>
          tcExpr(t.expr, TBoolean)
          val t1 = tcExpr(t.thn)
          val t2 = t.els.map(tcExpr(_)).getOrElse(TUnit)
          (t1.isSubTypeOf(t2), t2.isSubTypeOf(t1)) match {
            case (true,_) => t1
            case (_,true) => t2
            case _ =>
              Reporter.error(s"Type error. Then and Else arms of if-statements do not type match", expr)
              TUnit
          }
        case t: While =>
          tcExpr(t.cond, TBoolean)
          tcExpr(t.body, TUnit)
          TUnit
        case t: Println =>
          tcExpr(t.expr, TBoolean, TInt, TString)
          TUnit
        case t: Assign =>
          tcExpr(t.expr, t.id.getType)
          TUnit
        case t: Identifier => t.getType
      }

      expr.setType(tpe)

      if (expected.nonEmpty && !expected.exists(tpe.isSubTypeOf)) {
        Reporter.error(s"Type error. Expected ${expected.toList.mkString(" or ")}, found ${tpe}", expr)
        return expected.head
      }

      tpe
    }

    def tcVarDecl(v: VarDecl) = tcExpr(v.expr, v.tpe.getType)

    prog.classes.foreach(c => {
      c.vars.foreach(tcVarDecl)
      c.methods.foreach(m => {
        m.vars.foreach(tcVarDecl)
        m.exprs.foreach(tcExpr(_))
        tcExpr(m.retExpr, m.retType.getType)
      })
    })
    prog.main.vars.foreach(tcVarDecl)
    prog.main.exprs.foreach(tcExpr(_))

    prog
  }

}
