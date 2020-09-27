package punkt0
package analyzer

import Symbols._

object Types {

  trait Typed {
    private var _tpe: Type = TUntyped

    def setType(tpe: Type): this.type = { _tpe = tpe; this }
    def getType: Type = _tpe
    def getTypeStr: String = {
      try getType.toString
      catch {
        case t: Throwable => "??"
      }
    }
  }

  sealed abstract class Type {
    def isSubTypeOf(tpe: Type): Boolean
  }

  case object TError extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = true
    override def toString = "[error]"
  }

  case object TUntyped extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = false
    override def toString = "[untyped]"
  }

  case object TInt extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TInt => true
      case _ => false
    }
    override def toString = "Int"
  }
  // TODO: Complete by creating necessary types
  case class TAnyRef(classSymbol: ClassSymbol) extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = ???
    override def toString = classSymbol.name
  }


  // special object to implement the fact that all objects are its subclasses
  val anyRef = TAnyRef(new ClassSymbol("AnyRef"))
}
