package punkt0
package analyzer

import ast.Trees._
import Symbols._
import Types._

object NameAnalysis extends Phase[Program, Program] {

  def collectSymbols(prog: Program): GlobalScope = {

    def checkCyclicInheritance(classes: List[ClassSymbol]) = {
      var visited = classes.filter(_.parent.isEmpty).toSet

      def visit(c: ClassSymbol, path: List[ClassSymbol]): Unit = {
        if (path.contains(c)) {
          Reporter.error(s"Cyclic inheritance detected with class '${c.name}'", c)
          return
        }

        if (visited(c))
          return

        visited = visited + c
        c.parent.foreach(visit(_, c :: path))
      }

      classes.foreach(visit(_, List()))
    }

    def toVarMap(vars: List[VariableSymbol]): Map[String, VariableSymbol] =
      vars.foldLeft(Map[String, VariableSymbol]()) { (map, v) => map.contains(v.name) match {
        case true =>
          Reporter.error(s"Duplicate variable name '${v.name}'", v)
          map
        case false => map + (v.name -> v)
      }}

    def symbolizeVariable(v: VarDecl): VariableSymbol = {
      val symbol = new VariableSymbol(v.id.value).setPos(v)
      v.setSymbol(symbol)
      symbol.setType(v.tpe.getType)
      symbol
    }

    def symbolizeFormal(v: Formal): VariableSymbol = {
      val symbol = new VariableSymbol(v.id.value).setPos(v)
      v.setSymbol(symbol)
      symbol.setType(v.tpe.getType)
      symbol
    }

    def symbolizeMethod(m: MethodDecl, c: ClassSymbol): MethodSymbol = {
      var symbol = new MethodSymbol(m.id.value, c).setPos(m)
      val args = m.args.map(symbolizeFormal)
      val vars = m.vars.map(symbolizeVariable)

      symbol.argList = args
      symbol.params = toVarMap(args)
      symbol.members = toVarMap(vars)

      symbol.params.keySet
        .intersect(symbol.members.keySet)
        .foreach(k => Reporter.error(s"Parameter '$k' redeclared as variable", symbol.members(k)))

      m.setSymbol(symbol)
      symbol
    }

    def symbolizeClass(c: ClassDecl) = {
      val symbol = c.getSymbol
      symbol.members = toVarMap(c.vars.map(symbolizeVariable))
      val methodSymbols = c.methods.map(symbolizeMethod(_, symbol))
      symbol.methods = methodSymbols.foldLeft(Map[String, MethodSymbol]()) { (map, v) => map.contains(v.name) match {
        case true =>
          Reporter.error(s"Duplicate method name '${v.name}'", v)
          map
        case false => map + (v.name -> v)
      }}
      symbol.setType(TAnyRef(symbol))
    }

    def symbolizeMain(m: MainDecl): ClassSymbol = {
      val symbol = new ClassSymbol(m.obj.value).setPos(m)
      symbol.members = toVarMap(m.vars.map(symbolizeVariable))
      m.setSymbol(symbol)
      if (m.parent.value != "App")
        Reporter.error("Main has to extend 'App'", m)
      symbol
    }

    // symbolize all classes, methods, and vars
    var global = new GlobalScope()
    val classSymbols = prog.classes.map(c => c.setSymbol(new ClassSymbol(c.id.value)).getSymbol.setPos(c))
    prog.classes.foreach(symbolizeClass)
    global.mainClass = symbolizeMain(prog.main)

    global.classes = classSymbols.foldLeft(Map[String, ClassSymbol]()) { (map, c) => map.contains(c.name) match {
      case true =>
        Reporter.error(s"Duplicate class name '${c.name}'", c)
        map
      case false => map + (c.name -> c)
    }}

    // link parent classes
    prog.classes.foreach { c => c.getSymbol.parent = c.parent.flatMap(id => global.lookupClass(id.value)) }

    // link overridden methods
    prog.classes.foreach { c => c.methods.filter(_.overrides).foreach { m =>
      m.getSymbol.overridden = c.getSymbol.parent.flatMap(_.lookupMethod(m.id.value))
      if (m.getSymbol.overridden.isEmpty)
        Reporter.error(s"Method '${m.id.value}' marked as override but the method does not exist in parent", m.getSymbol)
    }}

    // find methods with duplicate names in parent, not marked as override
    // and methods marked as override, with no corresponding method in the parent
    prog.classes.filter(_.parent.isDefined).foreach { c =>
      val parent = c.getSymbol.parent.get
      c.methods
        .filter(m => !m.overrides && parent.lookupMethod(m.id.value).isDefined)
        .foreach(m => Reporter.error(s"Method '${m.id.value}' redeclared in child class and not marked as override", m.getSymbol))
      c.methods
        .filter(m => m.overrides && parent.lookupMethod(m.id.value).isEmpty)
        .foreach(m => Reporter.error(s"Method '${m.id.value}' declared as override but not found in parent class", m.getSymbol))
    }

    checkCyclicInheritance(global.mainClass :: classSymbols)

    global
  }

  def attachSymbols(scope: Scope, tree: Tree): Unit = tree match {
    case t: Program =>
      attachSymbols(scope.including(t.main), t.main)
      t.classes.foreach(c => attachSymbols(scope.including(c), c))
    case t: MainDecl =>
      attachSymbols(scope, t.obj)
      t.vars.foreach(attachSymbols(scope, _))
      t.exprs.foreach(attachSymbols(scope, _))
    case t: ClassDecl =>
      attachSymbols(scope, t.id)
      t.parent.foreach(attachSymbols(scope, _))
      t.vars.foreach(attachSymbols(scope, _))
      t.methods.foreach(m => attachSymbols(scope.including(m), m))
    case t: MethodDecl =>
      attachSymbols(scope, t.retType)
      attachSymbols(scope, t.retExpr)
      attachSymbols(scope, t.id)
      t.args.foreach(attachSymbols(scope, _))
      t.vars.foreach(attachSymbols(scope, _))
      t.exprs.foreach(attachSymbols(scope, _))
    case t: MethodCall =>
      attachSymbols(scope, t.obj)
      t.args.foreach(attachSymbols(scope, _))
    case t: VarDecl =>
      attachSymbols(scope, t.tpe)
      attachSymbols(scope, t.id)
      attachSymbols(scope, t.expr)
    case t: Formal =>
      attachSymbols(scope, t.tpe)
      attachSymbols(scope, t.id)
    case t: And =>
      attachSymbols(scope, t.lhs)
      attachSymbols(scope, t.rhs)
    case t: Or =>
      attachSymbols(scope, t.lhs)
      attachSymbols(scope, t.rhs)
    case t: Plus =>
      attachSymbols(scope, t.lhs)
      attachSymbols(scope, t.rhs)
    case t: Minus =>
      attachSymbols(scope, t.lhs)
      attachSymbols(scope, t.rhs)
    case t: Times =>
      attachSymbols(scope, t.lhs)
      attachSymbols(scope, t.rhs)
    case t: Div =>
      attachSymbols(scope, t.lhs)
      attachSymbols(scope, t.rhs)
    case t: LessThan =>
      attachSymbols(scope, t.lhs)
      attachSymbols(scope, t.rhs)
    case t: Equals =>
      attachSymbols(scope, t.lhs)
      attachSymbols(scope, t.rhs)
    case t: New =>
      attachSymbols(scope, t.tpe)
    case t: Not =>
      attachSymbols(scope, t.expr)
    case t: Block =>
      t.exprs.foreach(attachSymbols(scope, _))
    case t: If =>
      attachSymbols(scope, t.expr)
      attachSymbols(scope, t.thn)
      t.els.foreach(attachSymbols(scope, _))
    case t: While =>
      attachSymbols(scope, t.cond)
      attachSymbols(scope, t.body)
    case t: Println =>
      attachSymbols(scope, t.expr)
    case t: Assign =>
      if (scope.methodScope.flatMap(_.params.get(t.id.value)).isDefined)
        Reporter.error(s"Reassignment of method parameter '${t.id.value}'", t.id)
      attachSymbols(scope, t.id)
      attachSymbols(scope, t.expr)
    case t: This => t.setSymbol(scope.classScope.get)
    case t: Identifier => scope.lookupIdentifier(t.value) match {
      case Some(symbol) => t.setSymbol(symbol)
      case None => Reporter.error(s"Use of undefined variable '${t.value}'", t)
    }
    case _ => {}
  }

  def run(prog: Program)(ctx: Context): Program = {
    val global = collectSymbols(prog)
    attachSymbols(new Scope(globalScope = global), prog)
    prog.classes.foreach(_.methods.foreach(m => m.getSymbol.retType = m.retType.getType))
    prog
  }

}
