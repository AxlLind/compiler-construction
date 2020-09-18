package punkt0
package analyzer

object Symbols {

  trait Symbolic[S <: Symbol] {
    private var _sym: Option[S] = None

    def setSymbol(sym: S): this.type = {
      _sym = Some(sym)
      this
    }

    def getSymbol: S = _sym match {
      case Some(s) => s
      case None => sys.error("Accessing undefined symbol.")
    }

    def maybeSymbol: Option[S] = _sym
  }

  sealed abstract class Symbol extends Positioned {
    val id: Int = ID.next
    val name: String
  }

  private object ID {
    private var c: Int = 0

    def next: Int = {
      c = c + 1
      c
    }
  }

  class GlobalScope {
    var mainClass: ClassSymbol = _
    var classes = Map[String, ClassSymbol]()

    def lookupClass(n: String): Option[ClassSymbol] = {
      if (mainClass.name == n)
        return Some(mainClass)
      classes.get(n)
    }
  }

  class ClassSymbol(val name: String) extends Symbol {
    var parent: Option[ClassSymbol] = None
    var methods = Map[String, MethodSymbol]()
    var members = Map[String, VariableSymbol]()

    def lookupMethod(n: String): Option[MethodSymbol] =
      methods.get(n).orElse(parent.flatMap(_.lookupMethod(n)))

    def lookupVar(n: String): Option[VariableSymbol] =
      members.get(n).orElse(parent.flatMap(_.lookupVar(n)))
  }

  class MethodSymbol(val name: String, val classSymbol: ClassSymbol) extends Symbol {
    var params = Map[String, VariableSymbol]()
    var members = Map[String, VariableSymbol]()
    var argList: List[VariableSymbol] = Nil
    var overridden: Option[MethodSymbol] = None

    def lookupVar(n: String): Option[VariableSymbol] = {
      params.get(n)
        .orElse(members.get(n))
        //.orElse(overridden)
        .orElse(classSymbol.lookupVar(n))
    }
  }

  class VariableSymbol(val name: String) extends Symbol

}
