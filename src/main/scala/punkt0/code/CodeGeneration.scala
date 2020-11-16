package punkt0
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{New => _, _}
import ByteCodes._

object CodeGeneration extends Phase[Program, Unit] {
  sealed trait VariableType
  case object ClassField extends VariableType
  case class MethodArg(pos: Int) extends VariableType
  case class LocalVar(pos: Int) extends VariableType

  class CodeEmitter {
    var map: Map[VariableSymbol, VariableType] = Map()
    var className: String = null
    var ch: CodeHandler = null

    def withClass(c: ClassDecl): this.type = {
      className = c.getSymbol.name
      c.vars foreach { map += _.getSymbol -> ClassField }
      this
    }

    def withLocalVars(vars: List[VarDecl]): this.type = {
      vars.zipWithIndex foreach { case (arg, i) => map += arg.getSymbol -> LocalVar(ch.getFreshVar) }
      this
    }

    def withMethod(m: MethodDecl): CodeEmitter = {
      val newEmitter = new CodeEmitter()
      newEmitter.className = this.className
      newEmitter.map = this.map
      m.args.zipWithIndex foreach { case (arg, i) => newEmitter.map += arg.getSymbol -> MethodArg(i + 1) }
      newEmitter
    }

    def withHandler(ch: CodeHandler): this.type = {
      this.ch = ch
      this
    }

    def getVar(id: Identifier) = {
      val varSymbol = id.getSymbol.asInstanceOf[VariableSymbol]
      map.get(varSymbol).get match {
        case ClassField => ch << ALoad(0) << GetField(className, id.value, id.getType.typeSignature)
        case MethodArg(i) => ch << ArgLoad(i)
        case LocalVar(i) => varSymbol.getType match {
          case TBoolean | TInt => ch << ILoad(i)
          case _ => ch << ALoad(i)
        }
      }
    }

    def putVar(id: Identifier, expr: ExprTree) = {
      val varSymbol = id.getSymbol.asInstanceOf[VariableSymbol]
      map.get(varSymbol).get match {
        case ClassField =>
          ch << ALoad(0)
          emitCode(expr)
          ch << PutField(className, id.value, id.getType.typeSignature)
        case LocalVar(i) =>
          emitCode(expr)
          varSymbol.getType match {
            case TBoolean | TInt => ch << IStore(i)
            case _ => ch << AStore(i)
          }
        case MethodArg(i) => throw new Error("Assignment to method arg should not happen")
      }
    }

    def emitCode(expr: ExprTree): Unit = {
      ch << LineNumber(expr.line)
      expr match {
        case t: And =>
          val retFalse = ch.getFreshLabel("and_false")
          val end = ch.getFreshLabel("and_end")
          emitCode(t.lhs)           //   a
          ch << IfEq(retFalse)      //   if (!a) goto retFalse
          emitCode(t.rhs)           //   b
          ch << IfEq(retFalse)      //   if (!b) goto retFalse
          ch << Ldc(1) << Goto(end) //   true; goto end
          ch << Label(retFalse)     // retFalse:
          ch << Ldc(0)              //   false
          ch << Label(end)          // end:

        case t: Or =>
          val retTrue = ch.getFreshLabel("or_true")
          val end = ch.getFreshLabel("or_end")
          emitCode(t.lhs)           //   a
          ch << IfNe(retTrue)       //   if (a) goto retTrue
          emitCode(t.rhs)           //   b
          ch << IfNe(retTrue)       //   if (b) goto retTrue
          ch << Ldc(0) << Goto(end) //   false; goto end
          ch << Label(retTrue)      // retTrue:
          ch << Ldc(1)              //   true
          ch << Label(end)          // end:

        case t: Plus => (t.lhs.getType, t.rhs.getType) match {
          case (TInt, TInt) =>
            emitCode(t.lhs)
            emitCode(t.rhs)
            ch << IADD
          case (t1, t2) =>
            ch << DefaultNew("java/lang/StringBuilder")                                                                 // sb = new StringBuilder()
            emitCode(t.lhs)                                                                                             // { lhs }
            ch << InvokeVirtual("java/lang/StringBuilder", "append", s"(${t1.typeSignature})Ljava/lang/StringBuilder;") // sb.append(lhs)
            emitCode(t.rhs)                                                                                             // { rhs }
            ch << InvokeVirtual("java/lang/StringBuilder", "append", s"(${t2.typeSignature})Ljava/lang/StringBuilder;") // sb.append(rhs)
            ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")                          // sb.toString()
        }

        case t: Minus =>
          emitCode(t.lhs)
          emitCode(t.rhs)
          ch << ISUB
        case t: Times =>
          emitCode(t.lhs)
          emitCode(t.rhs)
          ch << IMUL
        case t: Div =>
          emitCode(t.lhs)
          emitCode(t.rhs)
          ch << IDIV

        case t: LessThan =>
          val le = ch.getFreshLabel("le")
          val end = ch.getFreshLabel("le_end")
          emitCode(t.lhs)              //   { lhs }
          emitCode(t.rhs)              //   { rhs }
          ch << If_ICmpLe(le)          //   if (lhs < rhs) goto le
          ch << Ldc(0)                 //   false
          ch << Goto(end)              //   goto end
          ch << Label(le)              // le:
          ch << Ldc(1)                 //   true
          ch << Label(end)             // end:

        case t: Equals =>
          val notEq = ch.getFreshLabel("not_eq")
          val end = ch.getFreshLabel("eq_end")

          val classComparison = t.lhs.getType.isSubTypeOf(anyRef)
          val branchInst = if (classComparison) If_ACmpNe(notEq) else If_ICmpNe(notEq)

          emitCode(t.lhs)              // { lhs }
          emitCode(t.rhs)              // { rhs }
          ch << branchInst             // if (lhs != rhs) goto notEq
          ch << Ldc(1) << Goto(end)    // return true
          ch << Label(notEq) << Ldc(0) // notEq: return false
          ch << Label(end)             // end:

        case t: Not =>
          emitCode(t.expr)     // { expr }
          ch << Ldc(1) << IXOR // expr ^ 1

        case t: MethodCall =>
          emitCode(t.obj)
          t.args foreach emitCode
          val signature = t.meth.getSymbol.asInstanceOf[MethodSymbol].signature()
          ch << InvokeVirtual(t.obj.getType.toString, t.meth.value, signature)

        case t: IntLit => ch << Ldc(t.value)
        case t: StringLit => ch << Ldc(t.value)
        case t: True => ch << Ldc(1)
        case t: False => ch << Ldc(0)
        case t: This => ch << ALoad(0)
        case t: Null => ch << ACONST_NULL
        case t: New => ch << DefaultNew(t.tpe.value)
        case t: Identifier => getVar(t)
        case t: Block => t.exprs foreach emitCode
        case t: Assign => putVar(t.id, t.expr)

        case t: If =>
          val labelElse = ch.getFreshLabel("if_else")
          val labelEnd = ch.getFreshLabel("if_end")
          emitCode(t.expr)       //   { expr }
          ch << IfEq(labelElse)  //   if (!expr) goto else
          emitCode(t.thn)        //   { body }
          ch << Goto(labelEnd)   //   goto end
          ch << Label(labelElse) // else:
          t.els foreach emitCode //   { els }
          ch << Label(labelEnd)  // end:

        case t: While =>
          val retry = ch.getFreshLabel("while_retry")
          val end = ch.getFreshLabel("while_end")
          ch << Label(retry) // retry:
          emitCode(t.cond)   //   { cond }
          ch << IfEq(end)    //   if (!cond) goto end
          emitCode(t.body)   //   { body }
          ch << Goto(retry)  //   goto retry
          ch << Label(end)   // end:

        case t: Println =>
          ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
          t.expr.getType match {
            case TString => emitCode(t.expr)
            case tpe => {
              ch << DefaultNew("java/lang/StringBuilder")                                                                  // sb = new StringBuilder()
              emitCode(t.expr)                                                                                             // { lhs }
              ch << InvokeVirtual("java/lang/StringBuilder", "append", s"(${tpe.typeSignature})Ljava/lang/StringBuilder;") // sb.append(lhs)
              ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")                           // sb.toString()
            }
          }
          ch << InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V")
      }
    }
  }


  def run(prog: Program)(ctx: Context): Unit = {
    def generateMain(main: MainDecl, sourceName: String, dir: String): Unit = {
      val classFile = new cafebabe.ClassFile("Main", None)
      classFile.setSourceFile(sourceName)
      classFile.addDefaultConstructor
      val ch = classFile.addMainMethod.codeHandler
      val emitter = new CodeEmitter().withHandler(ch).withLocalVars(main.vars)
      main.vars foreach { v => emitter.putVar(v.id, v.expr) }
      main.exprs foreach { expr =>
        emitter.emitCode(expr)
        if (expr.getType != TUnit) ch << POP
      }
      ch << RETURN
      // println("Main: -----------")
      // ch.print
      ch.freeze

      classFile.writeToFile(s"${dir}/Main.class")
    }

    def generateClassFile(c: ClassDecl, sourceName: String, dir: String): Unit = {
      val className = c.id.value
      val parent = c.parent map { _.value } getOrElse "java/lang/Object"
      val classFile = new cafebabe.ClassFile(className, Some(parent))
      classFile.setSourceFile(sourceName)
      c.vars foreach { v => classFile.addField(v.tpe.getType.typeSignature, v.id.value) }

      val constructorCH = classFile.addConstructor(Nil).codeHandler
      val classEmitter = new CodeEmitter().withClass(c).withHandler(constructorCH)
      constructorCH << ALOAD_0
      constructorCH << InvokeSpecial(parent, "<init>", "()V")
      c.vars foreach { v => classEmitter.putVar(v.id, v.expr) }
      constructorCH << RETURN
      println("CONSTRUCTOR ------")
      constructorCH.print
      constructorCH.freeze

      c.methods foreach { m =>
        val retSignature = m.retType.getType.typeSignature
        val argSignature = m.args map { _.tpe.getType.typeSignature }
        val ch = classFile.addMethod(retSignature, m.id.value, argSignature: _*).codeHandler
        val emitter = classEmitter.withMethod(m).withHandler(ch).withLocalVars(m.vars)
        for ((k,v) <- emitter.map) println(s"| ${k.name} -> $v")

        m.vars foreach { v => emitter.putVar(v.id, v.expr) }
        m.exprs foreach { expr =>
          emitter.emitCode(expr)
          if (expr.getType != TUnit) ch << POP
        }
        emitter.emitCode(m.retExpr)
        m.retType match {
          case BooleanType() | IntType() => ch << IRETURN
          case StringType() | Identifier(_) => ch << ARETURN
          case UnitType() => ch << RETURN
        }
        println(s"${m.id.value}: -----------")
        ch.print
        ch.freeze
      }

      classFile.writeToFile(s"${dir}/${className}.class")
    }

    val sourceName = ctx.file.get.getName
    val outDir = ctx.outDir map { _.getPath } getOrElse "."

    val f = new java.io.File(outDir)
    if (!f.exists()) { f.mkdir() }

    prog.classes foreach { generateClassFile(_, sourceName, outDir) }
    generateMain(prog.main, sourceName, outDir)
  }

}
