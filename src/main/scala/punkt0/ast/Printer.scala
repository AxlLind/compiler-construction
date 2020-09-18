package punkt0
package ast

import Trees._
import analyzer.Symbols._

object Printer {
  def asString(t: Tree, ctx: Context): String = {
    def indented(t: Tree, i: Int): String = s"${"  " * i}${apply(t, i)}"
    def applyArgList(args: List[Tree], i: Int): String =
      args.map(apply(_, i)).mkString(", ")
    def applyVarList(vars: List[VarDecl], i: Int): String =
      vars.map(indented(_, i+1)).mkString("\n") + (if (vars.isEmpty) "" else "\n\n")
    def applyExprList(exprs: List[ExprTree], i: Int): String =
      exprs.map(indented(_, i+1)).mkString(";\n") + (if (exprs.isEmpty) "" else "\n")
    def applyClassList(classes: List[ClassDecl], i: Int): String =
      classes.map(apply(_, i)).mkString("\n\n") + (if (classes.isEmpty) "" else "\n\n")
    def applySymbol(s: Option[Symbol]): String = (ctx.doSymbolIds, s) match {
      case (true, Some(symbol)) => s"#${symbol.id}"
      case (true, None) => "#??"
      case (false,_) => ""
    }

    def apply(tree: Tree, i: Int = 0): String = tree match {
      case t: Program => s"${applyClassList(t.classes, i)}${apply(t.main, i)}\n"
      case t: MainDecl =>
        val vars = applyVarList(t.vars, i)
        val exprs = applyExprList(t.exprs, i)
        s"object ${apply(t.obj)} extends ${apply(t.parent)} {\n${vars}${exprs}${"  " * i}}"
      case t: ClassDecl =>
        val parent = t.parent.map(x => s"extends ${apply(x)} ").getOrElse("")
        val vars = applyVarList(t.vars, i)
        val methods = t.methods.map(indented(_, i+1)).mkString("\n\n") + (if (t.methods.isEmpty) "" else "\n")
        s"class ${apply(t.id)} ${parent}{\n${vars}${methods}${"  " * i}}"
      case t: MethodDecl =>
        val overrides = if (t.overrides) s"override${applySymbol(t.getSymbol.overridden)} " else ""
        val args = applyArgList(t.args, i)
        val vars = applyVarList(t.vars, i)
        val exprs = applyExprList(t.exprs ++ List(t.retExpr), i)
        s"${overrides}def ${apply(t.id)}(${args}): ${apply(t.retType)} = {\n${vars}${exprs}${"  " * i}}"
      case t: MethodCall => s"${apply(t.obj, i)}.${apply(t.meth)}(${applyArgList(t.args, i)})"
      case t: VarDecl => s"var ${apply(t.id)}: ${apply(t.tpe)} = ${apply(t.expr, i)};"
      case t: Formal => s"${apply(t.id)}: ${apply(t.tpe)}"
      case t: And => s"(${apply(t.lhs, i)} && ${apply(t.rhs, i)})"
      case t: Or => s"(${apply(t.lhs, i)} || ${apply(t.rhs, i)})"
      case t: Plus => s"(${apply(t.lhs, i)} + ${apply(t.rhs, i)})"
      case t: Minus => s"(${apply(t.lhs, i)} - ${apply(t.rhs, i)})"
      case t: Times => s"(${apply(t.lhs, i)} * ${apply(t.rhs, i)})"
      case t: Div => s"(${apply(t.lhs, i)} / ${apply(t.rhs, i)})"
      case t: LessThan => s"(${apply(t.lhs, i)} < ${apply(t.rhs, i)})"
      case t: Equals   => s"(${apply(t.lhs, i)} == ${apply(t.rhs, i)})"
      case t: StringLit => s"${'"'}${t.value}${'"'}"
      case t: IntLit => t.value.toString
      case t: Identifier => s"${t.value}${applySymbol(t.maybeSymbol)}"
      case t: BooleanType => "Boolean"
      case t: IntType => "Int"
      case t: StringType => "String"
      case t: UnitType => "Unit"
      case t: True => "true"
      case t: False => "false"
      case t: This => s"this${applySymbol(t.maybeSymbol)}"
      case t: Null => "null"
      case t: New => s"new ${apply(t.tpe)}()"
      case t: Not => s"!${apply(t.expr, i)}"
      case t: Block => s"{\n${t.exprs.map(indented(_, i+1)).mkString(";\n")}\n${"  " * i}}"
      case t: If => s"if (${apply(t.expr, i)}) ${apply(t.thn, i)}${t.els.map(x => s" else ${apply(x, i)}").getOrElse("")}"
      case t: While => s"while (${apply(t.cond, i)}) ${apply(t.body, i)}"
      case t: Println => s"println(${apply(t.expr, i)})"
      case t: Assign => s"${apply(t.id)} = ${apply(t.expr, i)}"
    }

    apply(t)
  }
}
