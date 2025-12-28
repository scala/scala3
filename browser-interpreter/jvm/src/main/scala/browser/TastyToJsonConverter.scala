package browser

import scala.quoted.*
import scala.tasty.inspector.*

/**
 * Converts TASTy trees to JSON format for browser interpretation.
 *
 * Usage:
 *   TastyToJsonConverter.convert("path/to/file.tasty")
 */
object TastyToJsonConverter {

  /**
   * Convert TASTy files to JSON AST format.
   */
  def convert(tastyFiles: List[String]): String = {
    val results = new scala.collection.mutable.ListBuffer[String]()

    TastyInspector.inspectTastyFiles(tastyFiles)(new Inspector {
      def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit = {
        import quotes.reflect.*

        for (tasty <- tastys) {
          // Find main method
          object MainFinder extends TreeTraverser {
            override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match {
              case ddef @ DefDef("main", _, _, Some(rhs)) =>
                results += serializeTree(rhs)
              case _: PackageClause | _: ClassDef =>
                super.traverseTree(tree)(owner)
              case _ =>
            }
          }
          MainFinder.traverseTree(tasty.ast)(Symbol.spliceOwner)
        }

        def serializeTree(tree: Tree): String = {
          val sb = new StringBuilder()
          serialize(tree, sb)
          sb.toString
        }

        def serialize(tree: Tree, sb: StringBuilder): Unit = tree match {
          case Literal(const) =>
            sb.append(s"""{"tag":"Literal","type":"${constType(const)}","value":${constValue(const)}}""")

          case Ident(name) =>
            sb.append(s"""{"tag":"Ident","name":"${escape(name)}"}""")

          case Select(qualifier, name) =>
            sb.append("""{"tag":"Select","receiver":""")
            serialize(qualifier, sb)
            sb.append(s""","name":"${escape(name)}"}""")

          case Block(stats, expr) =>
            sb.append("""{"tag":"Block","stats":[""")
            var first = true
            for (stat <- stats if !isImport(stat)) {
              if (!first) sb.append(",")
              first = false
              serialize(stat, sb)
            }
            sb.append("""],"expr":""")
            serialize(expr, sb)
            sb.append("}")

          case If(cond, thenp, elsep) =>
            sb.append("""{"tag":"If","cond":""")
            serialize(cond, sb)
            sb.append(""","thenp":""")
            serialize(thenp, sb)
            sb.append(""","elsep":""")
            serialize(elsep, sb)
            sb.append("}")

          case While(cond, body) =>
            sb.append("""{"tag":"While","cond":""")
            serialize(cond, sb)
            sb.append(""","body":""")
            serialize(body, sb)
            sb.append("}")

          case Match(selector, cases) =>
            sb.append("""{"tag":"Match","selector":""")
            serialize(selector, sb)
            sb.append(""","cases":[""")
            cases.zipWithIndex.foreach { case (c, i) =>
              if (i > 0) sb.append(",")
              serializeCaseDef(c, sb)
            }
            sb.append("]}")

          case Try(block, catches, finalizer) =>
            sb.append("""{"tag":"Try","block":""")
            serialize(block, sb)
            sb.append(""","catches":[""")
            catches.zipWithIndex.foreach { case (c, i) =>
              if (i > 0) sb.append(",")
              serializeCaseDef(c, sb)
            }
            sb.append("]")
            finalizer match {
              case Some(f) =>
                sb.append(""","finalizer":""")
                serialize(f, sb)
              case None =>
            }
            sb.append("}")

          case Return(expr, from) =>
            sb.append("""{"tag":"Return","expr":""")
            serialize(expr, sb)
            sb.append("}")

          case Assign(lhs, rhs) =>
            val name = lhs match {
              case Ident(n) => n
              case _ => lhs.symbol.name
            }
            sb.append(s"""{"tag":"Assign","name":"${escape(name)}","rhs":""")
            serialize(rhs, sb)
            sb.append("}")

          case Apply(fn, args) =>
            fn match {
              case Select(qualifier, name) if isOperator(name) && args.size == 1 =>
                sb.append(s"""{"tag":"BinaryOp","op":"${escapeOp(name)}","lhs":""")
                serialize(qualifier, sb)
                sb.append(""","rhs":""")
                serialize(args.head, sb)
                sb.append("}")
              case Select(_, "<init>") =>
                val className = fn.symbol.owner.name
                sb.append(s"""{"tag":"New","class":"${escape(className)}","args":[""")
                args.zipWithIndex.foreach { case (arg, i) =>
                  if (i > 0) sb.append(",")
                  serialize(arg, sb)
                }
                sb.append("]}")
              case _ =>
                serializeApply(fn, args, sb)
            }

          case TypeApply(fn, targs) =>
            serialize(fn, sb)

          case Typed(expr, tpt) =>
            serialize(expr, sb)

          case ValDef(name, tpt, rhs) =>
            val tag = if (tree.symbol.flags.is(Flags.Mutable)) "VarDef" else "ValDef"
            sb.append(s"""{"tag":"$tag","name":"${escape(name)}","rhs":""")
            rhs match {
              case Some(r) => serialize(r, sb)
              case None => sb.append("null")
            }
            sb.append("}")

          case DefDef(name, paramss, returnTpt, rhs) =>
            sb.append(s"""{"tag":"DefDef","name":"${escape(name)}","params":[""")
            val params = paramss.flatMap {
              case clause: TermParamClause => clause.params.map(_.name)
              case _ => Nil
            }
            sb.append(params.map(p => s""""${escape(p)}"""").mkString(","))
            sb.append("""],"body":""")
            rhs match {
              case Some(r) => serialize(r, sb)
              case None => sb.append("null")
            }
            sb.append("}")

          // Handle closures - Block containing a DefDef and a Closure reference
          case Block(List(ddef: DefDef), Closure(_, _)) =>
            val params = ddef.termParamss.flatMap(_.params.map(_.name))
            sb.append("""{"tag":"Lambda","params":[""")
            sb.append(params.map(p => s""""${escape(p)}"""").mkString(","))
            sb.append("""],"body":""")
            ddef.rhs match {
              case Some(body) => serialize(body, sb)
              case None => sb.append("null")
            }
            sb.append("}")

          case New(tpt) =>
            val className = tpt.tpe.typeSymbol.name
            sb.append(s"""{"tag":"New","class":"${escape(className)}","args":[]}""")

          case This(qual) =>
            sb.append("""{"tag":"Ident","name":"this"}""")

          case Inlined(call, bindings, expansion) =>
            serialize(expansion, sb)

          case Repeated(elems, elemTpt) =>
            sb.append("""{"tag":"Apply","fn":{"tag":"Ident","name":"List"},"args":[""")
            elems.zipWithIndex.foreach { case (e, i) =>
              if (i > 0) sb.append(",")
              serialize(e, sb)
            }
            sb.append("]}")

          case _ =>
            sb.append(s"""{"tag":"Literal","type":"Unit","value":null,"_unsupported":"${tree.getClass.getSimpleName}"}""")
        }


        def serializeApply(fn: Tree, args: List[Tree], sb: StringBuilder): Unit = {
          sb.append("""{"tag":"Apply","fn":""")
          serialize(fn, sb)
          sb.append(""","args":[""")
          args.zipWithIndex.foreach { case (arg, i) =>
            if (i > 0) sb.append(",")
            serialize(arg, sb)
          }
          sb.append("]}")
        }

        def serializeCaseDef(caseDef: CaseDef, sb: StringBuilder): Unit = {
          sb.append("""{"pattern":""")
          serializePattern(caseDef.pattern, sb)
          caseDef.guard match {
            case Some(g) =>
              sb.append(""","guard":""")
              serialize(g, sb)
            case None =>
          }
          sb.append(""","body":""")
          serialize(caseDef.rhs, sb)
          sb.append("}")
        }

        def serializePattern(pattern: Tree, sb: StringBuilder): Unit = pattern match {
          case Wildcard() =>
            sb.append("""{"tag":"Wildcard"}""")

          case Bind(name, inner) =>
            sb.append(s"""{"tag":"Bind","name":"${escape(name)}","inner":""")
            serializePattern(inner, sb)
            sb.append("}")

          case Literal(const) =>
            sb.append(s"""{"tag":"Literal","value":${constValue(const)}}""")

          case Typed(expr, tpt) =>
            sb.append(s"""{"tag":"Typed","type":"${tpt.tpe.typeSymbol.name}","inner":""")
            serializePattern(expr, sb)
            sb.append("}")

          case TypedOrTest(inner, tpt) =>
            sb.append(s"""{"tag":"Typed","type":"${tpt.tpe.typeSymbol.name}","inner":""")
            serializePattern(inner, sb)
            sb.append("}")

          case Unapply(fun, implicits, patterns) =>
            val className = fun.symbol.owner.name
            sb.append(s"""{"tag":"Unapply","class":"${escape(className)}","patterns":[""")
            patterns.zipWithIndex.foreach { case (p, i) =>
              if (i > 0) sb.append(",")
              serializePattern(p, sb)
            }
            sb.append("]}")

          case Alternatives(patterns) =>
            sb.append("""{"tag":"Alternative","patterns":[""")
            patterns.zipWithIndex.foreach { case (p, i) =>
              if (i > 0) sb.append(",")
              serializePattern(p, sb)
            }
            sb.append("]}")

          case ref: Ident if ref.symbol.flags.is(Flags.Module) =>
            val name = ref.name
            if (name == "None" || name == "Nil") {
              sb.append(s"""{"tag":"Unapply","class":"$name","patterns":[]}""")
            } else {
              sb.append(s"""{"tag":"Literal","value":"$name"}""")
            }

          case Ident(name) =>
            sb.append(s"""{"tag":"Bind","name":"${escape(name)}"}""")

          case _ =>
            sb.append("""{"tag":"Wildcard","_unsupported":"true"}""")
        }

        def isImport(tree: Tree): Boolean = tree match {
          case _: Import => true
          case _ => false
        }

        def constType(const: Constant): String = const match {
          case IntConstant(_) => "Int"
          case LongConstant(_) => "Long"
          case FloatConstant(_) => "Float"
          case DoubleConstant(_) => "Double"
          case BooleanConstant(_) => "Boolean"
          case StringConstant(_) => "String"
          case UnitConstant() => "Unit"
          case NullConstant() => "Null"
          case CharConstant(_) => "Char"
          case _ => "Unknown"
        }

        def constValue(const: Constant): String = const match {
          case IntConstant(v) => v.toString
          case LongConstant(v) => v.toString
          case FloatConstant(v) => v.toString
          case DoubleConstant(v) => v.toString
          case BooleanConstant(v) => v.toString
          case StringConstant(v) => s""""${escape(v)}""""
          case UnitConstant() => "null"
          case NullConstant() => "null"
          case CharConstant(v) => s""""${escape(v.toString)}""""
          case _ => "null"
        }

        def escape(s: String): String = {
          s.flatMap {
            case '"' => "\\\""
            case '\\' => "\\\\"
            case '\n' => "\\n"
            case '\r' => "\\r"
            case '\t' => "\\t"
            case c => c.toString
          }
        }

        def escapeOp(op: String): String = op match {
          case "$plus" => "+"
          case "$minus" => "-"
          case "$times" => "*"
          case "$div" => "/"
          case "$percent" => "%"
          case "$less" => "<"
          case "$greater" => ">"
          case "$less$eq" => "<="
          case "$greater$eq" => ">="
          case "$eq$eq" => "=="
          case "$bang$eq" => "!="
          case "$amp$amp" => "&&"
          case "$bar$bar" => "||"
          case "$colon$colon" => "::"
          case _ => op
        }

        def isOperator(name: String): Boolean = {
          name match {
            case "+" | "-" | "*" | "/" | "%" | "<" | ">" | "<=" | ">=" | "==" | "!=" | "&&" | "||" | "::" => true
            case n if n.startsWith("$") => true
            case _ => false
          }
        }
      }
    })

    if (results.isEmpty) {
      """{"error": "No main method found"}"""
    } else {
      results.head
    }
  }

  /**
   * Convert a single TASTy file and print to stdout.
   */
  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println("Usage: TastyToJsonConverter <tasty-file>...")
      System.exit(1)
    }

    val json = convert(args.toList)
    println(json)
  }
}
