package punkt0
package ast

import Trees._

object Printer {
  def indented(t: Tree, i: Int): String = s"${"  " * i}${apply(t, i)}"

  def apply(t: Tree, i: Int = 0): String = t match {
    case p: Program =>
      val classes = p.classes.map(apply(_, i)).mkString("\n") + (if (p.classes.isEmpty) "" else "\n")
      s"${classes}${apply(p.main, i)}\n"
    case p: MainDecl =>
      val vars = p.vars.map(indented(_,i+1)).mkString("\n") + (if (p.vars.isEmpty) "" else "\n")
      val exprs = p.exprs.map(indented(_, i+1)).mkString("\n") + (if (p.exprs.isEmpty) "" else "\n")
      s"class ${apply(p.obj)} extends ${apply(p.parent)} {\n${vars}${exprs}${"  " * i}}"
    case p: ClassDecl =>
      val parent = p.parent.map(x => s"extends ${apply(x)} ").getOrElse("")
      val vars = p.vars.map(indented(_,i+1)).mkString("\n") + (if (p.vars.isEmpty) "" else "\n")
      val methods = p.methods.map(indented(_, i+1)).mkString("\n") + (if (p.methods.isEmpty) "" else "\n")
      s"class ${apply(p.id)} ${parent}{\n${vars}${methods}${"  " * i}}"
    case p: MethodDecl =>
      val overrides = if (p.overrides) "override " else ""
      val args = p.args.map(apply(_)).mkString(", ")
      val vars = p.vars.map(indented(_,i+1)).mkString("\n") + (if (p.vars.isEmpty) "" else "\n")
      val exprs = p.exprs.map(indented(_,i+1)).mkString(";\n") + (if (p.exprs.isEmpty) "" else "\n")
      s"${overrides}def ${apply(p.id)}(${args}): ${apply(p.retType)} = {\n${vars}${exprs}${indented(p.retExpr,i+1)};\n${"  " * i}}"
    case p: MethodCall =>
      val args = p.args.map(apply(_,i)).mkString(",")
      s"${apply(p.obj, i)}.${apply(p.meth)}(${args})"
    case p: VarDecl => s"var ${apply(p.id)}: ${apply(p.tpe)} = ${apply(p.expr, i)}};"
    case p: Formal => s"${apply(p.id)}: ${apply(p.tpe)}"
    case p: And => s"${apply(p.lhs, i)} && ${apply(p.rhs, i)}"
    case p: Or => s"${apply(p.lhs, i)} || ${apply(p.rhs, i)}"
    case p: Plus => s"${apply(p.lhs, i)} + ${apply(p.rhs, i)}"
    case p: Minus => s"${apply(p.lhs, i)} - ${apply(p.rhs, i)}"
    case p: Times => s"${apply(p.lhs, i)} * ${apply(p.rhs, i)}"
    case p: Div => s"${apply(p.lhs, i)} / ${apply(p.rhs, i)}"
    case p: LessThan => s"${apply(p.lhs, i)} < ${apply(p.rhs, i)}"
    case p: Equals => s"${apply(p.lhs, i)} == ${apply(p.rhs, i)}"
    case p: StringLit => s"${'"'}${p.value}${'"'}"
    case p: IntLit => p.value.toString
    case p: Identifier => p.value
    case p: BooleanType => "Boolean"
    case p: IntType => "Int"
    case p: StringType => "String"
    case p: UnitType => "Unit"
    case p: True => "true"
    case p: False => "false"
    case p: This => "this"
    case p: Null => "null"
    case p: New => s"new ${apply(p.tpe)}()"
    case p: Not => s"!${apply(p.expr, i)}"
    case p: Block => s"{\n${p.exprs.map(indented(_, i+1)).mkString("\n")}\n${"  " * i}}"
    case p: If => s"if (${apply(p.expr, i)}) ${apply(p.thn, i)}${p.els.map(x => s" else ${apply(x,i)}").getOrElse("")}"
    case p: While => s"while (${apply(p.cond, i)}) ${apply(p.body, i)}"
    case p: Println => s"println(${apply(p.expr, i)})"
    case p: Assign => s"${apply(p.id)} = ${apply(p.expr, i)};"
  }
}
