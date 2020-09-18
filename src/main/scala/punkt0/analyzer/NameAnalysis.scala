package punkt0
package analyzer

import ast.Trees._
import Symbols._

object NameAnalysis extends Phase[Program, Program] {

  def collectSymbols(prog: Program): GlobalScope = {
    def toVarMap(vars: List[VariableSymbol]): Map[String, VariableSymbol] = {
      vars.foldLeft(Map[String, VariableSymbol]()) { (map, v) => {
        if (map.contains(v.name)) {
          Reporter.error(s"Duplicate variable name '${v.name}'", v)
          return map
        }
        map + (v.name -> v)
      }}
    }

    def toMethodMap(vars: List[MethodSymbol]): Map[String, MethodSymbol] = {
      vars.foldLeft(Map[String, MethodSymbol]()) { (map, v) => {
        if (map.contains(v.name)) {
          Reporter.error(s"Duplicate method name '${v.name}'", v)
          return map
        }
        map + (v.name -> v)
      }}
    }

    def toClassMap(classes: List[ClassSymbol]): Map[String, ClassSymbol] = {
      classes.foldLeft(Map[String, ClassSymbol]()) { (map, c) => {
        if (map.contains(c.name)) {
          Reporter.error(s"Duplicate class name '${c.name}'", c)
          return map
        }
        map + (c.name -> c)
      }}
    }

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
      val args = m.args.map(symbolizeFormal(_))

      symbol.argList = args
      symbol.params = toVarMap(args)
      symbol.members = toVarMap(m.vars.map(symbolizeVariable(_)))

      symbol.params.keySet
        .intersect(symbol.members.keySet)
        .foreach(k => Reporter.error(s"Parameter '$k' redeclared as variable", symbol.members(k)))

      m.setSymbol(symbol)
      symbol
    }

    def symbolizeClass(c: ClassDecl) = {
      c.getSymbol.members = toVarMap(c.vars.map(symbolizeVariable(_)))
      c.getSymbol.methods = toMethodMap(c.methods.map(symbolizeMethod(_, c.getSymbol)))
    }

    def symbolizeMain(m: MainDecl) = {
      m.setSymbol(new ClassSymbol(m.obj.value).setPos(m))
      m.getSymbol.members = toVarMap(m.vars.map(symbolizeVariable(_)))
      // todo m.parent
    }

    // symbolize all classes, methods, and vars
    var global = new GlobalScope()
    prog.classes.foreach(c => c.setSymbol(new ClassSymbol(c.id.value).setPos(c)))
    prog.classes.foreach(symbolizeClass)
    global.classes = toClassMap(prog.classes.map(_.getSymbol))

    symbolizeMain(prog.main)
    global.mainClass = prog.main.getSymbol

    // now that classes and methods have been symbolized, try to link extended classes and overridden methods
    prog.classes.foreach { c => c.getSymbol.parent = c.parent.flatMap(id => global.lookupClass(id.value)) }
    prog.classes.foreach { c =>
      c.methods.filter(_.overrides).foreach { m =>
        m.getSymbol.overridden = c.getSymbol.parent.flatMap(_.lookupMethod(m.id.value) match {
          case Some(symbol) => Some(symbol)
          case None =>
            Reporter.error(s"Method '${m.id.value}' marked as override but the method does not exist in parent", m.getSymbol)
            None
        })
      }
    }
    prog.classes.filter(_.parent.isDefined).foreach { c => {
      val parent = c.getSymbol.parent.get
      c.methods.filter(!_.overrides).filter(m => parent.lookupMethod(m.id.value).isDefined).foreach(m =>
        Reporter.error(s"Method '${m.id.value}' redeclared in child class and not marked as override", m.getSymbol)
      )
    }}

    global
  }

  case class Scope(
    globalScope: Option[GlobalScope] = None,
    classScope: Option[ClassSymbol] = None,
    methodScope: Option[MethodSymbol] = None,
  ) {
    def lookupClass(n: String): Option[ClassSymbol] =
      globalScope.flatMap(_.lookupClass(n))

    def lookupMethod(n: String): Option[MethodSymbol] =
      classScope.flatMap(_.lookupMethod(n))

    def lookupVar(n: String): Option[VariableSymbol] =
      methodScope.flatMap(_.lookupVar(n))
        .orElse(classScope.flatMap(_.lookupVar(n)))

    def lookupIdentifier(n: String): Option[Symbol] =
      lookupVar(n).orElse(lookupMethod(n)).orElse(lookupClass(n))
  }

  def attachSymbols(tree: Tree, scope: Scope): Unit = tree match {
    case t: Program =>
      attachSymbols(t.main, scope.copy(classScope = Some(t.main.getSymbol)))
      t.classes.foreach(c => attachSymbols(c, scope.copy(classScope = Some(c.getSymbol))))
    case t: MainDecl =>
      attachSymbols(t.obj, scope)
      // attachSymbols(t.parent, scope) // todo: should this be looked up?
      t.vars.foreach(attachSymbols(_, scope))
      t.exprs.foreach(attachSymbols(_, scope))
    case t: ClassDecl =>
      attachSymbols(t.id, scope)
      t.parent.foreach(attachSymbols(_, scope))
      t.vars.foreach(attachSymbols(_, scope))
      t.methods.foreach(m => attachSymbols(m, scope.copy(methodScope = Some(m.getSymbol))))
    case t: MethodDecl =>
      attachSymbols(t.retType, scope)
      attachSymbols(t.retExpr, scope)
      attachSymbols(t.id, scope)
      t.args.foreach(attachSymbols(_, scope))
      t.vars.foreach(attachSymbols(_, scope))
      t.exprs.foreach(attachSymbols(_, scope))
    case t: MethodCall =>
      attachSymbols(t.obj, scope)
      scope.classScope.get.lookupMethod(t.meth.value).foreach(t.meth.setSymbol(_))
      // attachSymbols(t.meth, scope)
      t.args.foreach(attachSymbols(_, scope))
    case t: VarDecl =>
      attachSymbols(t.tpe, scope)
      attachSymbols(t.id, scope)
      attachSymbols(t.expr, scope)
    case t: Formal =>
      attachSymbols(t.tpe, scope)
      attachSymbols(t.id, scope)
    case t: And =>
      attachSymbols(t.lhs, scope)
      attachSymbols(t.rhs, scope)
    case t: Or =>
      attachSymbols(t.lhs, scope)
      attachSymbols(t.rhs, scope)
    case t: Plus =>
      attachSymbols(t.lhs, scope)
      attachSymbols(t.rhs, scope)
    case t: Minus =>
      attachSymbols(t.lhs, scope)
      attachSymbols(t.rhs, scope)
    case t: Times =>
      attachSymbols(t.lhs, scope)
      attachSymbols(t.rhs, scope)
    case t: Div =>
      attachSymbols(t.lhs, scope)
      attachSymbols(t.rhs, scope)
    case t: LessThan =>
      attachSymbols(t.lhs, scope)
      attachSymbols(t.rhs, scope)
    case t: Equals =>
      attachSymbols(t.lhs, scope)
      attachSymbols(t.rhs, scope)
    case t: New =>
      attachSymbols(t.tpe, scope)
    case t: Not =>
      attachSymbols(t.expr, scope)
    case t: Block =>
      t.exprs.foreach(attachSymbols(_, scope))
    case t: If =>
      attachSymbols(t.expr, scope)
      attachSymbols(t.thn, scope)
      t.els.foreach(attachSymbols(_, scope))
    case t: While =>
      attachSymbols(t.cond, scope)
      attachSymbols(t.body, scope)
    case t: Println =>
      attachSymbols(t.expr, scope)
    case t: Assign =>
      if (scope.methodScope.flatMap(_.params.get(t.id.value)).isDefined) {
        Reporter.error(s"Reassignment of method parameter '${t.id.value}'", t.id)
      }
      attachSymbols(t.id, scope)
      attachSymbols(t.expr, scope)
    case t: This =>
      t.setSymbol(scope.classScope.get)
    case t: Identifier => scope.lookupIdentifier(t.value) match {
      case Some(symbol) => t.setSymbol(symbol)
      case None => Reporter.error(s"Use of undefined variable '${t.value}'", t)
    }
    case _ => {}
  }

  def run(prog: Program)(ctx: Context): Program = {
    val global = collectSymbols(prog)
    attachSymbols(prog, new Scope(globalScope = Some(global)))
    prog
  }

}
