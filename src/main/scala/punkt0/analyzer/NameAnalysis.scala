package punkt0
package analyzer

import ast.Trees._
import Symbols._
import Types._

object NameAnalysis extends Phase[Program, Program] {

  def collectSymbols(prog: Program): GlobalScope = {

    def checkCyclicInheritance(classes: List[ClassDecl]) = {
      var visited = Set[ClassSymbol]()

      def visit(c: ClassSymbol, path: List[ClassSymbol]): Unit = {
        if (path.contains(c)) {
          val cyclePath = (path.reverse.dropWhile(_ != c) ::: List(c)).map(_.name).mkString(" -> ")
          Reporter.fatal(s"Cyclic inheritance detected: ${cyclePath}", c)
        }

        if (visited(c))
          return

        visited = visited + c
        c.parent.foreach(visit(_, c :: path))
      }

      classes.map(_.getSymbol).foreach(visit(_, List()))
    }

    def toVarMap(vars: List[VariableSymbol]): Map[String, VariableSymbol] =
      vars.foldLeft(Map[String, VariableSymbol]()) { (map, v) => map.contains(v.name) match {
        case true =>
          Reporter.error(s"Variable '${v.name}' is already defined", v)
          map
        case false => map + (v.name -> v)
      }}

    def symbolizeVariable(v: VarDecl): VariableSymbol = {
      val symbol = new VariableSymbol(v.id.value).setPos(v)
      v.setSymbol(symbol)
      symbol
    }

    def symbolizeFormal(v: Formal): VariableSymbol = {
      val symbol = new VariableSymbol(v.id.value).setPos(v)
      v.setSymbol(symbol)
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

    var global = new GlobalScope()

    // create class symbols and check for duplicate class definitions
    global.classes = prog.classes
      .map(c => c.setSymbol(new ClassSymbol(c.id.value)).getSymbol.setPos(c))
      .foldLeft(Map[String, ClassSymbol]()) { (map, c) => map.contains(c.name) match {
        case true =>
          Reporter.error(s"Multiple definitions of class '${c.name}'", c)
          map
        case false => map + (c.name -> c)
      }}
    prog.classes.foreach(symbolizeClass)

    global.mainClass = symbolizeMain(prog.main)

    // link parent classes
    prog.classes
      .filter(_.parent.isDefined)
      .foreach { c => c.getSymbol.parent = global.lookupClass(c.parent.get.value) }
    checkCyclicInheritance(prog.classes)

    // check that fields don't override
    prog.classes.filter(_.getSymbol.parent.isDefined) foreach { c =>
      val parent = c.getSymbol.parent.get
      c.vars.filter(v => parent.lookupVar(v.id.value).isDefined) foreach { Reporter.error("Field overrides parent field", _) }
    }

    // link overridden methods
    prog.classes.foreach { c => c.methods.filter(_.overrides).foreach { m =>
      m.getSymbol.overridden = c.getSymbol.parent.flatMap(_.lookupMethod(m.id.value))
    }}

    // check override constraints
    prog.classes.filter(_.getSymbol.parent.isDefined) foreach { c =>
      c.methods foreach { m => (c.getSymbol.parent.get.lookupMethod(m.id.value), m.overrides) match {
        case (Some(parentMethod), true) =>
          if (parentMethod.argList.length != m.args.length)
            Reporter.error("Overridden method needs to have the same number of arguments.", m)
          m.getSymbol.overridden = Some(parentMethod)
        case (Some(parentMethod), false) =>
          if (parentMethod.argList.length == m.args.length)
            Reporter.error("Method matches a parent method but is not declared as override.", m)
        case (None, true) => Reporter.error("Method marked as override but parent has no corresponding method", m)
        case (None, false) => {}
      }}
    }

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
      case None =>
        // so that other calls do not fail, we exit anyway
        t.setSymbol(new VariableSymbol("[undefined]"))
        Reporter.error(s"Reference to undefined identifier.", t)
    }
    case _ => {}
  }

  def run(prog: Program)(ctx: Context): Program = {
    val global = collectSymbols(prog)
    attachSymbols(new Scope(globalScope = global), prog)
    prog.classes.foreach(_.methods foreach { m => m.getSymbol.retType = m.retType.getType })
    prog
  }

}
