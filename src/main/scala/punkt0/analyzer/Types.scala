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
    def isPrimitive: Boolean = false
  }

  case object TError extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = true
    override def toString = "[error]"
  }

  case object TUntyped extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = false
    override def toString = "[untyped]"
  }

  case object TBoolean extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe == TBoolean
    override def isPrimitive: Boolean = true
    override def toString = "Boolean"
  }

  case object TInt extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe == TInt
    override def isPrimitive: Boolean = true
    override def toString = "Int"
  }

  case object TString extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe == TString
    override def isPrimitive: Boolean = true
    override def toString = "String"
  }

  case object TUnit extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe == TUnit
    override def isPrimitive: Boolean = true
    override def toString = "Unit"
  }

  case object TNull extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TNull | TAnyRef(_) => true
      case _ => false
    }
    override def isPrimitive: Boolean = true
    override def toString = "Null"
  }

  case class TAnyRef(classSymbol: ClassSymbol) extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TAnyRef(c) =>
        tpe == anyRef ||
        c == classSymbol ||
        classSymbol.parent.map(_.getType.isSubTypeOf(tpe)).getOrElse(false)
      case _ => false
    }
    override def toString = classSymbol.name
  }

  // special object to implement the fact that all objects are its subclasses
  val anyRef = TAnyRef(new ClassSymbol("AnyRef"))
}
