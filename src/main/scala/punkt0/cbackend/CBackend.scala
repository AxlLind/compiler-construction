package punkt0
package cbackend

import java.io._
import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._

object CBackend extends Phase[Program, Unit] {
  // A helper class to keep track of where to find references to variables.
  // These can either be a local variable, in which are easy to reference,
  // or a class field, in which we need to know at which parent the field
  // exists so we can cast it to a pointer of that type.
  case class VariableScope(
    c: ClassSymbol = null,
    m: Option[MethodSymbol] = None,
    isMain: Boolean = false,
  ) {
    def getClassVar(name: String, cls: ClassSymbol): Option[String] = {
      if (cls.members.get(name).isDefined)
        return Some(cls.name)
      cls.parent flatMap { getClassVar(name, _) }
    }

    def lookupVar(name: String): Option[String] = {
      if (isMain)
        return Some(name)
      if (m.isDefined) {
        val maybeMethodVar = m.get.params.get(name) orElse m.get.members.get(name)
        if (maybeMethodVar.isDefined)
          return Some(name)
      }
      getClassVar(name, c) map { s => s"((struct p0_$s*)this)->$name" }
    }

    def including[S <: Symbolic[_ <: Symbol]](s: S): VariableScope = s.getSymbol match {
      case cls: ClassSymbol => copy(c = cls)
      case meth: MethodSymbol => copy(m = Some(meth))
    }

    def withMain = copy(isMain = true)
  }

  // Each method has a unique index in the class' vtable, which
  // is used when looking up if the method in question has
  // been overwritten by a child class.
  def computeVTableIndexes(c: ClassDecl) = {
    var parent = c.getSymbol.parent
    var numParentMethods = 0
    while (parent.isDefined) {
      numParentMethods += parent.get.methods.size
      parent = parent.get.parent
    }
    c.getSymbol.numParentMethods = numParentMethods
    c.methods.zipWithIndex foreach { case (m, i) => m.getSymbol.vtableIndex = numParentMethods + i}
  }

  // Build the virtual method tables for all classes. From these you can tell at
  // runtime if the method has been overwritten, has if so call that method instead.
  // Essentially this is a just of function pointers stored as a void* array,
  // and cast to a function pointer of the correct type at runtime.
  // If the entry in the vtable is NULL then the method has not been overwritten.
  def buildVTables(classes: List[ClassDecl]): String = {
    def buildVTable(c: ClassDecl): String = {
      var classTree = List(c.getSymbol)
      var parent = c.getSymbol.parent
      while (parent.isDefined) {
        classTree = parent.get :: classTree
        parent = parent.get.parent
      }
      val overrideMap = classTree
        .flatMap(_.methods.values)
        .filter(_.overridden.isDefined)
        .map(m => m.overridden.get.vtableIndex -> s"p0_${c.id.value}_${m.name}")
        .toMap

      val numMethods = c.getSymbol.numParentMethods + c.methods.length
      val entries = (0 until numMethods).map(i => overrideMap.get(i).getOrElse("NULL"))
      s"void *p0_${c.id.value}__vtable[] = { ${entries mkString ", "} };"
    }
    s"/*----- vtables -----*/\n${classes.map(buildVTable).mkString("\n")}"
  }

  def toCCode(prog: Program): String = {
    def indented(scope: VariableScope, t: Tree, i: Int): String = s"${"  " * i}${apply(scope, t, i)}"

    def methodSignature(scope: VariableScope, m: MethodDecl): String = {
      val className = m.getSymbol.classSymbol.name
      val argList = m.args map { apply(scope, _) } mkString ", "
      val argListStr = if (argList.isEmpty) "" else s", $argList"
      s"${apply(scope, m.retType)} p0_${className}_${m.id.value}(struct p0_$className *this$argListStr)"
    }

    // Figuring out which functions will reference which and which
    // classes is difficult, so just forward declare everything.
    def forwardDecls(scope: VariableScope, p: Program): String = {
      val classes = p.classes map { c => s"struct p0_${c.id.value};" } mkString "\n"
      val initFunctions = p.classes map { c => s"void p0_${c.id.value}__init(struct p0_${c.id.value} *this);"} mkString "\n"
      val newFunctions = p.classes map { c => s"${apply(scope, c.id)} p0_${c.id.value}__new();"} mkString "\n"
      val methods = p.classes
        .map(cls => {
          val newScope = scope including cls
          cls.methods map { m => s"${methodSignature(newScope including m, m)};"} mkString "\n"
        })
        .filter(!_.isEmpty)
        .mkString("\n")

      s"/*----- forward declarations -----*/\n$classes\n$initFunctions\n$newFunctions\n$methods"
    }

    def typeToStr(tpe: Type): String = tpe match {
      case TBoolean => "int"
      case TInt => "int"
      case TString => "char*"
      case TAnyRef(c) => s"struct p0_${c.name}*"
      case _ => throw new Error("Unreachable")
    }

    // Definitions of all the classes in the program as structs. If the class
    // does not inherit from anything it needs to store a pointer to the virtual
    // table in it's struct definition, otherwise it needs to keep a parent type.
    def structDefinitions(scope: VariableScope, classes: List[ClassDecl]): String = {
      def classToStruct(c: ClassDecl): String = {
        val parent = c.parent map { p => s"  struct p0_${p.value} parent;\n" } getOrElse ""
        val vtablePtr = if (c.getSymbol.parent.isEmpty) "  void **vtable;\n" else ""
        val fields = c.vars map { v => s"  ${apply(scope, v.tpe)} ${v.id.value};" } mkString "\n"
        val fieldsStr = if (c.vars.isEmpty) "" else s"$fields\n"
        s"struct p0_${c.id.value} {\n$vtablePtr$parent$fieldsStr};"
      }
      s"/*----- struct definitions -----*/\n${classes.map(classToStruct).mkString("\n")}"
    }

    def apply(scope: VariableScope, tree: Tree, i: Int = 0): String = tree match {
      case t: Program =>
        val forwardDeclarations = forwardDecls(scope, t)
        val vtables = buildVTables(t.classes)
        val structs = structDefinitions(scope, t.classes)
        val classMethods = t.classes map { c => apply(scope including c, c) } mkString "\n\n"
        val p0Main = apply(scope.withMain including t.main, t.main)
        val cMain = List(
          s"int main() {",
          s"  u8 dummy;",
          s"  gc_init(&dummy);",
          s"  p0_main();",
          s"}",
        ).mkString("\n")
        List(forwardDeclarations, vtables, structs, classMethods, p0Main, cMain) mkString "\n\n"

      // mark as noinline to force a stack frame to be allocated, this
      // means the stack-size will always be non-zero, which helps the GC.
      case t: MainDecl =>
        val vars = t.vars map { apply(scope, _, 1) } mkString ";\n  "
        val varsStr = if (t.vars.isEmpty) "" else s"  $vars;\n"
        val exprs = t.exprs map { apply(scope, _, 1) } mkString ";\n  "
        s"/*----- punkt0 main function -----*/\nvoid __attribute__((noinline)) p0_main() {\n$varsStr  $exprs;\n}"

      case t: ClassDecl =>
        val className = t.id.value
        val parentInit = t.parent map { p => s"  p0_${p.value}__init(this);\n" } getOrElse ""

        val varInits = t.vars map { v => s"  ${apply(scope, v.id)} = ${apply(scope, v.expr)}"} mkString ";\n"
        val varInitsStr = if (t.vars.isEmpty) "" else s"$varInits;\n"

        // cast this to a vtable pointer and assign it
        val linkVtable = s"  *((void***)this) = p0_${className}__vtable;\n"
        val initFn = s"void p0_${className}__init(struct p0_${className} *this) {\n$parentInit$linkVtable$varInitsStr}"

        val tpe = apply(scope, t.id)
        val constructor = List(
          s"$tpe p0_${className}__new() {",
          s"  $tpe this = gc_malloc(sizeof(struct p0_${className}));",
          s"  p0_${className}__init(this);",
          s"  return this;",
          s"}",
        ).mkString("\n")
        val methods = t.methods map { m => apply(scope including m, m) } mkString "\n\n"
        val methodsStr = if (t.methods.isEmpty) "" else s"\n\n$methods"
        s"/*----- $className member functions -----*/\n$initFn\n\n$constructor$methodsStr"

      case t: MethodDecl =>
        val retType = apply(scope, t.retType)
        val args = if (t.args.isEmpty) "" else s", ${t.args map { _.id.value } mkString ", "}"

        // check if the method has been overwritten, and if so use that version instead
        val overrideCheck = List(
          s"  void *_override_ptr = (*(void***)this)[${t.getSymbol.vtableIndex}];",
          s"  if (_override_ptr != NULL)",
          s"    return (($retType (*)())_override_ptr)(this$args);"
        ).mkString("\n")

        val argList = t.args map { apply(scope, _) } mkString ", "
        val argListStr = if (argList.isEmpty) "" else s", $args"

        val vars = t.vars map { apply(scope, _, 1) } mkString ";\n  "
        val varsStr = if (t.vars.isEmpty) "" else s"  $vars;\n"

        val exprs = t.exprs map { apply(scope, _, 1) } mkString ";\n  "
        val exprsStr = if (t.exprs.isEmpty) "" else s"  $exprs;\n"
        val maybeReturn = if (t.retType == UnitType()) "" else "return ";
        s"${methodSignature(scope, t)} {\n$overrideCheck\n$varsStr$exprsStr  gc_collect();\n  $maybeReturn${apply(scope, t.retExpr)};\n}"

      case t: VarDecl  => s"${apply(scope, t.tpe)} ${t.id.value} = ${apply(scope, t.expr)}"
      case t: Formal   => s"${apply(scope, t.tpe)} ${apply(scope, t.id)}"
      case t: And      => s"(${apply(scope, t.lhs)} && ${apply(scope, t.rhs)})"
      case t: Or       => s"(${apply(scope, t.lhs)} || ${apply(scope, t.rhs)})"
      case t: Minus    => s"(${apply(scope, t.lhs)} - ${apply(scope, t.rhs)})"
      case t: Times    => s"(${apply(scope, t.lhs)} * ${apply(scope, t.rhs)})"
      case t: Div      => s"(${apply(scope, t.lhs)} / ${apply(scope, t.rhs)})"
      case t: LessThan => s"(${apply(scope, t.lhs)} < ${apply(scope, t.rhs)})"
      case t: Equals   => s"(${apply(scope, t.lhs)} == ${apply(scope, t.rhs)})"

      // Use the str addition functions for the overloaded plus operator.
      // These are defined in the gc.h file which is always included.
      case t: Plus =>
        val lhs = apply(scope, t.lhs);
        val rhs = apply(scope, t.rhs);
        (t.lhs.getType, t.rhs.getType) match {
          case (TInt, TInt) => s"($lhs + $rhs)"
          case (TInt, TString) => s"str_add_is($lhs, $rhs)"
          case (TString, TInt) => s"str_add_si($lhs, $rhs)"
          case (TString, TString) => s"str_add($lhs, $rhs)"
          case _ => throw new Error("unreachable")
        }
      case t: MethodCall =>
        val args = t.args map { apply(scope, _, i) } mkString ", "
        val argStr = if (t.args.isEmpty) "" else s", $args"
        s"${apply(scope, t.meth)}(${apply(scope, t.obj)}$argStr)"
      case t: IntLit => s"${t.value}"
      case t: StringLit => s"${'"'}${t.value}${'"'}"
      case t: True => "1"
      case t: False => "0"
      case t: This => "this"
      case t: Null => "NULL"
      case t: New => s"p0_${t.tpe.value}__new()"
      case t: Not => s"!${apply(scope, t.expr, i)}"
      case t: Identifier => t.getSymbol match {
        case c: ClassSymbol => s"struct p0_${t.value}*"
        case m: MethodSymbol => s"p0_${m.classSymbol.name}_${t.value}"
        case v: VariableSymbol => scope.lookupVar(t.value).get
      }
      case t: BooleanType => "int"
      case t: IntType => "int"
      case t: StringType => "char*"
      case t: UnitType => "void"
      case t: Block =>
        val block = s"{\n${t.exprs.map(indented(scope, _, i+1)).mkString(";\n")};\n${"  " * i}}"
        t.getType match {
          case TUnit => block
          case _ => s"($block)"
        }
      case t: If => t.getType match {
        case TUnit => s"if (${apply(scope, t.expr, i)}) {\n  ${apply(scope, t.thn, i)};\n}${t.els.map(x => s" else {\n  ${apply(scope, x, i)};\n}").getOrElse("")}"
        case tpe => {
          val thn = s"  p0_if_res = ${apply(scope, t.thn, i)};"
          val els = s"  p0_if_res = ${apply(scope, t.els.get, i)};"
          s"({\n  ${typeToStr(tpe)} p0_if_res;\n  if (${apply(scope, t.expr, i)}) {\n  $thn\n  } else {\n $els\n}\n  p0_if_res;\n})"
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
    val gcFile = new java.io.File("./c-backend/gc.h")
    val gcFileStr = scala.io.Source.fromFile(gcFile).mkString

    val outDir = ctx.outDir map { _.getPath } getOrElse "."
    val f = new java.io.File(outDir)
    if (!f.exists()) { f.mkdir() }

    prog.classes foreach computeVTableIndexes

    val outName = ctx.file.get.getName
    val pw = new PrintWriter(new File(s"${outDir}/${outName}.c"))
    pw.write(s"$gcFileStr\n${toCCode(prog)}")
    pw.close
  }

}
