package punkt0
package cbackend

import java.io._
import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._

object CBackend extends Phase[Program, Unit] {
  case class VariableScope(
    c: ClassSymbol = null,
    m: Option[MethodSymbol] = None,
    isMain: Boolean = false,
  ) {
    def getClassVar(name: String, cls: ClassSymbol): Option[String] = {
      if (cls.members.get(name).isDefined)
        return Some(name)
      cls.parent flatMap { getClassVar(name, _) } map { s => s"parent.$s" }
    }

    def lookupVar(name: String): Option[String] = {
      if (isMain)
        return Some(name)
      if (m.isDefined) {
        val maybeMethodVar = m.get.params.get(name) orElse m.get.members.get(name)
        if (maybeMethodVar.isDefined)
          return Some(name)
      }
      getClassVar(name, c) map { s => s"this->$s"}
    }

    def including[S <: Symbolic[_ <: Symbol]](s: S): VariableScope = s.getSymbol match {
      case cls: ClassSymbol => copy(c = cls)
      case meth: MethodSymbol => copy(m = Some(meth))
    }

    def withMain = copy(isMain = true)
  }

  def toCCode(prog: Program): String = {
    def indented(scope: VariableScope, t: Tree, i: Int): String = s"${"  " * i}${apply(scope, t, i)}"

    def methodSignature(scope: VariableScope, m: MethodDecl): String = {
      val className = m.getSymbol.classSymbol.name
      val argList = m.args map { apply(scope, _) } mkString ", "
      val argListStr = if (argList.isEmpty) "" else s", $argList"
      s"${apply(scope, m.retType)} punkt0_${className}_${m.id.value}(struct punkt0_$className *this$argListStr)"
    }

    def forwardDecls(scope: VariableScope, p: Program): String = {
      val classes = p.classes map { c => s"struct punkt0_${c.id.value};" } mkString "\n"
      val initFunctions = p.classes map { c => s"void punkt0_init_${c.id.value}(struct punkt0_${c.id.value} *this);"} mkString "\n"
      val newFunctions = p.classes map { c => s"${apply(scope, c.id)} punkt0_new_${c.id.value}();"} mkString "\n"
      val methods = p.classes map { cls =>
        val newScope = scope including cls
        cls.methods map { m => s"${methodSignature(newScope including m, m)};"} mkString "\n"
      } mkString "\n"

      s"$classes\n$initFunctions\n$newFunctions\n$methods"
    }

    def typeToStr(tpe: Type): String = tpe match {
      case TBoolean => "int"
      case TInt => "int"
      case TString => "char *"
      case TAnyRef(c) => s"struct punkt0_${c.name} *"
      case _ => throw new Error("Unreachable")
    }

    def classToStruct(scope: VariableScope, c: ClassDecl): String = {
      // (id: Identifier, parent: Option[Identifier], vars: List[VarDecl], methods: List[MethodDecl])
      val parent = c.parent map { p => s"  struct punkt0_${p.value} parent;\n" } getOrElse ""
      val vtable = if (c.methods.isEmpty) "" else s"  void *vtable[${c.methods.length}];\n"
      val fields = c.vars map { v => s"  ${apply(scope, v.tpe)} ${v.id.value};" } mkString "\n"
      val fieldsStr = if (c.vars.isEmpty) "" else s"$fields\n"
      s"struct punkt0_${c.id.value} {\n$parent$vtable$fieldsStr};"
    }

    def setupVtables(scope: VariableScope, c: ClassDecl): String = {
      val overriddenMethods = c.methods.filter { _.overrides }
      val vtableOverrides = overriddenMethods map { m =>
        val symbol = m.getSymbol
        val overridden = symbol.overridden.get
        val parentClass = overridden.classSymbol.name
        s"  ((struct punkt0_$parentClass*)this)->vtable[${overridden.index}] = ${apply(scope, m.id)};"
      }
      if (vtableOverrides.isEmpty) "" else s"${vtableOverrides mkString "\n"}\n"
    }

    def apply(scope: VariableScope, tree: Tree, i: Int = 0): String = tree match {
      case t: Program =>
        val forwardDeclarations = forwardDecls(scope, t)
        val structDefinitions = t.classes map { classToStruct(scope, _) } mkString "\n\n"
        val classMethods = t.classes map { c => apply(scope including c, c) } mkString "\n\n"
        val main = apply(scope.withMain including t.main, t.main)
        List(forwardDeclarations, structDefinitions, classMethods, main) mkString "\n\n"

      case t: MainDecl =>
        val vars = t.vars map { apply(scope, _, 1) } mkString ";\n  "
        val exprs = t.exprs map { apply(scope, _, 1) } mkString ";\n  "
        s"int main() {\n  u8 dummy_sp;\n  gc_init(&dummy_sp);\n  $vars;\n  $exprs;\n}"

      case t: ClassDecl =>
        // struct definition is handled by Program, just have to add methods and constructor
        val parentInit = t.parent map { p => s"  punkt0_init_${p.value}(this);\n" } getOrElse ""

        val varInits = t.vars map { v => s"  ${apply(scope, v.id)} = ${apply(scope, v.expr)}"} mkString ";\n"
        val varInitsStr = if (t.vars.isEmpty) "" else s"$varInits;\n"

        val initFunction = s"void punkt0_init_${t.id.value}(struct punkt0_${t.id.value} *this) {\n$parentInit${setupVtables(scope, t)}$varInitsStr}"

        val thisType = apply(scope, t.id)
        val constructor = s"$thisType punkt0_new_${t.id.value}() {\n  $thisType this = gc_malloc(sizeof(struct punkt0_${t.id.value}));\n  punkt0_init_${t.id.value}(this);\n  return this;\n}"
        val methods = t.methods map { m => apply(scope including m, m) } mkString "\n\n"
        s"$initFunction\n\n$constructor\n\n$methods\n"

      case t: MethodDecl =>
        // (overrides: Boolean, retType: TypeTree, id: Identifier, args: List[Formal], vars: List[VarDecl], exprs: List[ExprTree], retExpr: ExprTree)
        val retType = apply(scope, t.retType)
        val args = if (t.args.isEmpty) "" else s", ${t.args map { _.id.value } mkString ", "}"
        val methodIndex = t.getSymbol.index
        val overrideCheck = s"if (this->vtable[$methodIndex] != NULL)\n    return (($retType (*)())this->vtable[$methodIndex])(this$args);"

        val argList = t.args map { apply(scope, _) } mkString ", "
        val argListStr = if (argList.isEmpty) "" else s", $args"

        val vars = t.vars map { apply(scope, _, 1) } mkString ";\n  "
        val varsStr = if (t.vars.isEmpty) "" else s"  $vars;\n"

        val exprs = t.exprs map { apply(scope, _, 1) } mkString ";\n  "
        val exprsStr = if (t.exprs.isEmpty) "" else s"  $exprs;\n"
        val maybeReturn = if (t.retType == UnitType()) "" else "return ";
        s"${methodSignature(scope, t)} {\n  $overrideCheck\n$varsStr$exprsStr  $maybeReturn${apply(scope, t.retExpr)};\n}"

      case t: VarDecl  => s"${apply(scope, t.tpe)} ${t.id.value} = ${apply(scope, t.expr)}"
      case t: Formal   => s"${apply(scope, t.tpe)} ${apply(scope, t.id)}"
      case t: And      => s"(${apply(scope, t.lhs)} && ${apply(scope, t.rhs)})"
      case t: Or       => s"(${apply(scope, t.lhs)} || ${apply(scope, t.rhs)})"
      case t: Minus    => s"(${apply(scope, t.lhs)} - ${apply(scope, t.rhs)})"
      case t: Times    => s"(${apply(scope, t.lhs)} * ${apply(scope, t.rhs)})"
      case t: Div      => s"(${apply(scope, t.lhs)} / ${apply(scope, t.rhs)})"
      case t: LessThan => s"(${apply(scope, t.lhs)} < ${apply(scope, t.rhs)})"
      case t: Equals   => s"(${apply(scope, t.lhs)} == ${apply(scope, t.rhs)})"

      case t: Plus =>
        val lhs = apply(scope, t.lhs);
        val rhs = apply(scope, t.rhs);
        (t.lhs.getType, t.rhs.getType) match {
          case (TInt, TInt) => s"($lhs + $rhs)"
          case (TInt, TString) => s"str_add_is($lhs, $rhs)"
          case (TString, TInt) => s"str_add_si($lhs, $rhs)"
          case (TString, TString) => s"str_add_ss($lhs, $rhs)"
          case _ => throw new Error("unreachable")
        }
      case t: MethodCall =>
        val args = t.args map { apply(scope, _, i) } mkString ", "
        val argStr = if (t.args.isEmpty) "" else s", $args"
        val methodsClass = t.meth.getSymbol.asInstanceOf[MethodSymbol].classSymbol.name;
        s"${apply(scope, t.meth)}(${apply(scope, t.obj)}$argStr)"
      case t: IntLit => s"${t.value}"
      case t: StringLit => s"${'"'}${t.value}${'"'}"
      case t: True => "1"
      case t: False => "0"
      case t: This => "this"
      case t: Null => "NULL"
      case t: New => s"punkt0_new_${t.tpe.value}()"
      case t: Not => s"!${apply(scope, t.expr, i)}"
      case t: Identifier => t.getSymbol match {
        case c: ClassSymbol => s"struct punkt0_${t.value}*"
        case m: MethodSymbol => s"punkt0_${m.classSymbol.name}_${t.value}"
        case v: VariableSymbol => scope.lookupVar(t.value).get
      }
      case t: BooleanType => "int"
      case t: IntType => "int"
      case t: StringType => "char *"
      case t: UnitType => "void"
      case t: Block => s"{\n${t.exprs.map(indented(scope, _, i+1)).mkString(";\n")};\n${"  " * i}}"
      case t: If => t.getType match {
        case TUnit => s"if (${apply(scope, t.expr, i)}) {\n  ${apply(scope, t.thn, i)};\n}${t.els.map(x => s" else {\n  ${apply(scope, x, i)};\n}").getOrElse("")}"
        case tpe => {
          val thn = s"  punkt0_if_res = ${apply(scope, t.thn, i)};"
          val els = s"  punkt0_if_res = ${apply(scope, t.els.get, i)};"
          s"({\n  ${typeToStr(tpe)} punkt0_if_res;\n  if (${apply(scope, t.expr, i)}) {\n  $thn\n  } else {\n $els\n}\n  punkt0_if_res;\n})"
        }
      }
      case t: While => s"while (${apply(scope, t.cond, i)}) ${apply(scope, t.body, i)}"
      case t: Println =>
        val (printStr, printValue) = t.expr.getType match {
          case TString => ("%s", apply(scope, t.expr, i))
          case TInt => ("%d", apply(scope, t.expr, i))
          case TBoolean => ("%s", s"${apply(scope, t.expr, i)} ? ${'"'}true${'"'} : ${'"'}false${'"'}")
          case _ => throw new Error("unreachable")
        }
        s"printf(${'"'}$printStr\\n${'"'}, $printValue)"
      case t: Assign => s"${apply(scope, t.id)} = ${apply(scope, t.expr, i)}"
    }

    apply(new VariableScope(), prog)
  }

  def run(prog: Program)(ctx: Context): Unit = {
    val outDir = ctx.outDir map { _.getPath } getOrElse "."

    val f = new java.io.File(outDir)
    if (!f.exists()) { f.mkdir() }

    println(toCCode(prog))
    // val pw = new PrintWriter(new File(s"${outDir}/out.c"))
    // pw.write(toCCode(prog))
    // pw.close
  }

}
