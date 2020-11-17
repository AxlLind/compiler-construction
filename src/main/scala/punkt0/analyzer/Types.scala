package punkt0
package analyzer

import scala.util.Try
import Symbols._

object Types {

  trait Typed {
    private var _tpe: Type = TUntyped

    def setType(tpe: Type): this.type = { _tpe = tpe; this }
    def getType: Type = _tpe
    def getTypeStr: String = Try(getType.toString).getOrElse("??")
  }

  sealed abstract class Type {
    def isSubTypeOf(tpe: Type): Boolean
    def jvmType: String = ???
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
    override def toString = "Boolean"
    override def jvmType = "Z"
  }

  case object TInt extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe == TInt
    override def toString = "Int"
    override def jvmType = "I"
  }

  case object TString extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe == TString
    override def toString = "String"
    override def jvmType = "Ljava/lang/String;"
  }

  case object TUnit extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe == TUnit
    override def toString = "Unit"
    override def jvmType = "V"
  }

  case object TNull extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TNull | TAnyRef(_) => true
      case _ => false
    }
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
    override def jvmType = s"L${classSymbol.name};"
  }

  // special object to implement the fact that all objects are its subclasses
  val anyRef = TAnyRef(new ClassSymbol("AnyRef"))
}
