package punkt0
package analyzer

import ast.Trees._

import Symbols._
import Types._

object TypeChecking extends Phase[Program, Program] {

  def run(prog: Program)(ctx: Context): Program = {
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
        case t: Equals => (tcExpr(t.lhs), tcExpr(t.rhs)) match {
          case (TInt, TInt)
             | (TUnit, TUnit)
             | (TString, TString)
             | (TBoolean, TBoolean)
             | (TAnyRef(_), TAnyRef(_)) => TBoolean
          case (t1,t2) => typeError(s"The equals-operator is not defined for $t1 == $t2", expr)
        }
        case t: Plus => (tcExpr(t.lhs), tcExpr(t.rhs)) match {
          case (TInt, TString)
             | (TString, TInt)
             | (TString, TString) => TString
          case (TInt, TInt) => TInt
          case (t1,t2) => typeError(s"The plus-operator is not defined for $t1 + $t2", expr)
        }
        case t: While =>
          tcExpr(t.cond, TBoolean)
          tcExpr(t.body, TUnit)
        case t: Println =>
          tcExpr(t.expr, TBoolean, TInt, TString)
          TUnit
        case t: Assign =>
          tcExpr(t.expr, t.id.getType)
          TUnit
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
            case None => typeError(s"Class '${c.name}' has no method '${t.meth.value}'.", expr)
          }
          case _ => typeError(s"Method '${t.meth.value}' called on a expression that is not a class.", expr)
        }
        case t: If =>
          tcExpr(t.expr, TBoolean)
          val t1 = tcExpr(t.thn)
          val t2 = t.els.map(tcExpr(_)).getOrElse(TUnit)
          (t1.isSubTypeOf(t2), t2.isSubTypeOf(t1)) match {
            case (true,_) => t1
            case (_,true) => t2
            case _ => typeError(s"Then and Else arms of if-statements do not type match", expr)
          }
      }

      expr.setType(tpe)

      if (expected.nonEmpty && !expected.exists(tpe.isSubTypeOf)) {
        typeError(s"Expected ${expected.toList.mkString(" or ")}, found ${tpe}", expr)
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
