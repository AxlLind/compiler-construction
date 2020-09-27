package punkt0
package analyzer

import ast.Trees._

object TypedASTPrinter {
    val indentSize = 2
    var depth = 0

    def indent():String = {
        if (depth > 0) "\n" + (" " * depth * indentSize) else ""
    }

    def handleList(l: List[Tree]):String = {
        var s = indent + "["
        depth += 1
        s += l.map(x => apply(x)).mkString(",")
        depth -= 1
        s += indent + "]"
        s
    }

    def methodId(id:Identifier):String = {
        indent + "Identifier(" + id.value + "#" + id.getSymbol.id + ")"
    }

    def apply(t:Tree):String = {
        var s = indent
        var doIndent:Boolean = true
        depth = depth + 1
        s += (t match {
            case Program(main, classes)=>
                "Program(" + apply(main) + "," + handleList(classes) 
            case MainDecl(obj, parent, vars, exprs)=>
                "MainDecl(" + apply(obj) + "," + handleList(vars) + "," + handleList(exprs)
            case ClassDecl(id, parent, vars, methods)=>
                "ClassDecl(" + apply(id) + "," + handleList(vars) + ","  + handleList(methods)
            case VarDecl(tpe, id, expr)=>
                "VarDecl(" + apply(tpe)+ "," + apply(id) + "," + apply(expr)
            case MethodDecl(overrides, retType, id, args, vars, exprs, retExpr)=>
                "MethodDecl(" + overrides + "," + apply(retType) + "," + methodId(id) + "," + handleList(args) + "," + handleList(vars) + "," + handleList(exprs) + "," + apply(retExpr)
            case Formal(tpe, id) => "Formal(" + apply(tpe) + "," + apply(id)
            case BooleanType()=>
                doIndent = false
                "BooleanType("
            case IntType()=>
                doIndent = false
                "IntType("
            case StringType()=>
                doIndent = false
                "StringType("
            case UnitType()=>
                doIndent = false
                "UnitType("
            case And(lhs, rhs)=>
                "And(" + apply(lhs) + "," + apply(rhs)
            case Or(lhs, rhs)=>
                "Or(" + apply(lhs) + "," + apply(rhs)
            case Plus(lhs, rhs)=>
                "Plus(" + apply(lhs) + "," + apply(rhs)
            case Minus(lhs, rhs)=>
                "Minus(" + apply(lhs) + "," + apply(rhs)
            case Times(lhs, rhs)=>
                "Times(" + apply(lhs) + "," + apply(rhs)
            case Div(lhs, rhs)=>
                "Div(" + apply(lhs) + "," + apply(rhs)
            case LessThan(lhs, rhs)=>
                "LessThan(" + apply(lhs) + "," + apply(rhs)
            case Equals(lhs, rhs)=>
                "Equals(" + apply(lhs) + "," + apply(rhs)
            case MethodCall(obj, meth, args)=>
                "MethodCall(" + apply(obj) + "," + methodId(meth) + "," + handleList(args)
            case IntLit(value)=>
                doIndent = false
                "IntLit(" + value
            case StringLit(value)=>
                doIndent = false
                "StringLit(" + value
            case True()=>
                doIndent = false
                "True("
            case False()=>
                doIndent = false
                "False("
            case id @ Identifier(value)=>
                doIndent = false
                "Identifier(" + value + "#" + id.getSymbol.id
            case This()=>
                doIndent = false
                "This("
            case Null()=>
                doIndent = false
                "Null("
            case New(tpe)=>
                "New(" + apply(tpe)
            case Not(expr)=>
                "Not(" + apply(expr)
            case Block(exprs)=>
                "Block(" + handleList(exprs)
            case If(expr, thn, els)=>
                "If(" + apply(expr) + "," + apply(thn) + (
                    els match {
                        case Some(t) =>  "," + apply(t)
                        case _ => ""
                    }
                )
            case While(cond, body)=>
                "While(" + apply(cond) + "," + apply(body)
            case Println(expr)=>
                "Println(" + apply(expr)
            case Assign(id, expr)=>
                "Assign(" + apply(id) + "," + apply(expr)
        })
        depth = depth - 1
        s += (if (doIndent) indent else "") + ")"
        s += (if (t.isInstanceOf[ExprTree]) {":" + t.asInstanceOf[ExprTree].getTypeStr} else {""})
        s
  }
}