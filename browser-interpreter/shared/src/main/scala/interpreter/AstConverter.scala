package interpreter

import dotc.ast.{Trees => T}
import dotc.core.Names._
import dotc.core.Constants.Constant

/**
 * Converts parser AST (dotc.ast.Trees.Tree) to interpreter AST (interpreter.Ast).
 *
 * This allows us to parse Scala source code and directly interpret it
 * without going through type checking or TASTy generation.
 */
object AstConverter {

  /** Convert a list of trees (compilation unit) to interpreter AST */
  def convert(trees: List[T.Tree]): List[Ast] = trees.flatMap(convertTree)

  /** Convert a single tree to interpreter AST */
  def convertTree(tree: T.Tree): Option[Ast] = tree match {
    // Module/Object definition
    case T.ModuleDef(name, template) =>
      val body = convertTemplate(template)
      Some(Ast.Block(body, Ast.UnitLit))

    // Class definition - for now, just convert to block with constructor
    case T.ClassDef(name, tparams, template) =>
      val body = convertTemplate(template)
      Some(Ast.Block(body, Ast.UnitLit))

    // Package - just extract the statements
    case T.PackageDef(pid, stats) =>
      val converted = stats.flatMap(convertTree)
      Some(Ast.Block(converted.init, converted.lastOption.getOrElse(Ast.UnitLit)))

    // Import - skip for now (interpreter doesn't need imports)
    case T.Import(_, _) => None

    // Value definition
    case vd @ T.ValDef(name, tpt, rhs) if !rhs.isEmpty =>
      Some(Ast.ValDef(name.toString, convertExpr(rhs), vd.mods.is(dotc.core.Flags.Mutable)))

    case T.ValDef(name, tpt, _) =>
      // Uninitialized val - use null/unit
      Some(Ast.ValDef(name.toString, Ast.NullLit, false))

    // Method definition
    case T.DefDef(name, paramss, tpt, rhs) if name.toString != "<init>" =>
      val params = paramss.flatMap {
        case T.TermParamClause(ps) => ps.map(p => p.name.toString)
        case T.TypeParamClause(_) => Nil
        case _ => Nil
      }
      val body = if (rhs.isEmpty) Ast.UnitLit else convertExpr(rhs)
      Some(Ast.DefDef(name.toString, params, body))

    // Constructor - skip
    case T.DefDef(name, _, _, _) if name.toString == "<init>" => None

    // Type definition - skip (interpreter doesn't need types)
    case T.TypeDef(_, _) => None

    // Expression
    case expr => Some(convertExpr(expr))
  }

  /** Convert template body */
  private def convertTemplate(template: T.Template): List[Ast] = {
    template.body.flatMap(convertTree)
  }

  /** Convert expression tree to interpreter AST */
  def convertExpr(tree: T.Tree): Ast = tree match {
    // Literals
    case T.Literal(const) => convertConstant(const)

    // Identifier
    case T.Ident(name) =>
      if (name.toString == "_") Ast.Ident("_")
      else Ast.Ident(name.toString)

    // Selection (x.y)
    case T.Select(qual, name) =>
      Ast.Select(convertExpr(qual), name.toString)

    // Application (f(args))
    case T.Apply(fun, args) =>
      Ast.Apply(convertExpr(fun), args.map(convertExpr))

    // Type application (f[T]) - just use the function
    case T.TypeApply(fun, _) =>
      convertExpr(fun)

    // Block { stats; expr }
    case T.Block(stats, expr) =>
      val convertedStats = stats.flatMap(convertTree)
      Ast.Block(convertedStats, convertExpr(expr))

    // If expression
    case T.If(cond, thenp, elsep) =>
      Ast.If(convertExpr(cond), convertExpr(thenp), convertExpr(elsep))

    // Match expression
    case T.Match(selector, cases) =>
      Ast.Match(convertExpr(selector), cases.map(convertCaseDef))

    // While loop
    case T.WhileDo(cond, body) =>
      Ast.While(convertExpr(cond), convertExpr(body))

    // For comprehension (yield)
    case T.ForYield(enums, body) =>
      // Convert to nested flatMap/map calls
      convertForComprehension(enums, body, isYield = true)

    // For loop (do)
    case T.ForDo(enums, body) =>
      // Convert to nested foreach calls
      convertForComprehension(enums, body, isYield = false)

    // Try/catch/finally
    case T.Try(block, cases, finalizer) =>
      val fin = if (finalizer.isEmpty) None else Some(convertExpr(finalizer))
      Ast.Try(convertExpr(block), cases.map(convertCaseDef), fin)

    // Return
    case T.Return(expr, _) =>
      Ast.Return(if (expr.isEmpty) Ast.UnitLit else convertExpr(expr))

    // Throw
    case T.Throw(expr) =>
      Ast.Throw(convertExpr(expr))

    // New instance
    case T.Apply(T.Select(T.New(tpt), _), args) =>
      Ast.New(typeToString(tpt), args.map(convertExpr))

    case T.New(tpt) =>
      Ast.New(typeToString(tpt), Nil)

    // Lambda/Function
    case T.Function(params, body) =>
      val paramNames = params.map {
        case vd: T.ValDef => vd.name.toString
        case other => "_"
      }
      Ast.Lambda(paramNames, convertExpr(body))

    // Infix operation (a op b)
    case T.InfixOp(left, op, right) =>
      Ast.BinaryOp(op.asInstanceOf[T.Ident].name.toString, convertExpr(left), convertExpr(right))

    // Prefix operation (!x, -x, etc.)
    case T.PrefixOp(op, operand) =>
      Ast.UnaryOp(op.asInstanceOf[T.Ident].name.toString, convertExpr(operand))

    // Parentheses
    case T.Parens(expr) =>
      convertExpr(expr)

    // Tuple
    case T.Tuple(elems) =>
      // Convert to Tuple class construction
      Ast.New(s"Tuple${elems.size}", elems.map(convertExpr))

    // Typed expression (e: T) - just use the expression
    case T.Typed(expr, _) =>
      convertExpr(expr)

    // Assignment (x = e)
    case T.Assign(lhs, rhs) =>
      lhs match {
        case T.Ident(name) => Ast.Assign(name.toString, convertExpr(rhs))
        case _ => Ast.Assign("<error>", convertExpr(rhs))
      }

    // Annotated expression - just use the expression
    case T.Annotated(arg, _) =>
      convertExpr(arg)

    // This reference
    case T.This(_) =>
      Ast.Ident("this")

    // Super reference
    case T.Super(_, _) =>
      Ast.Ident("super")

    // Empty tree
    case T.EmptyTree =>
      Ast.UnitLit

    // Val/Def in expression position
    case vd: T.ValDef =>
      convertTree(vd).getOrElse(Ast.UnitLit)

    case dd: T.DefDef =>
      convertTree(dd).getOrElse(Ast.UnitLit)

    // Fallback
    case _ =>
      // Unknown expression type - return error identifier
      Ast.Ident(s"<unknown:${tree.getClass.getSimpleName}>")
  }

  /** Convert a constant to interpreter AST */
  private def convertConstant(const: Constant): Ast = const.value match {
    case i: Int => Ast.IntLit(i)
    case l: Long => Ast.LongLit(l)
    case f: Float => Ast.FloatLit(f)
    case d: Double => Ast.DoubleLit(d)
    case b: Boolean => Ast.BoolLit(b)
    case c: Char => Ast.CharLit(c)
    case s: String => Ast.StringLit(s)
    case null => Ast.NullLit
    case () => Ast.UnitLit
    case _ => Ast.StringLit(const.value.toString)
  }

  /** Convert a case clause */
  private def convertCaseDef(caseDef: T.CaseDef): Ast.CaseDef = {
    val pattern = convertPattern(caseDef.pat)
    val guard = if (caseDef.guard.isEmpty) None else Some(convertExpr(caseDef.guard))
    val body = convertExpr(caseDef.body)
    Ast.CaseDef(pattern, guard, body)
  }

  /** Convert a pattern */
  private def convertPattern(tree: T.Tree): Ast.Pattern = tree match {
    case T.Ident(name) if name.toString == "_" =>
      Ast.Pattern.Wildcard

    case T.Ident(name) =>
      Ast.Pattern.Bind(name.toString)

    case T.Literal(const) =>
      Ast.Pattern.Literal(const.value)

    case T.Bind(name, body) =>
      val inner = if (body.isEmpty) None else Some(convertPattern(body))
      Ast.Pattern.Bind(name.toString, inner)

    case T.Typed(pat, tpt) =>
      val inner = if (pat.isEmpty) None else Some(convertPattern(pat))
      Ast.Pattern.Typed(typeToString(tpt), inner)

    case T.UnApply(fun, implicits, pats) =>
      val className = fun match {
        case T.Ident(n) => n.toString
        case T.Select(_, n) => n.toString
        case _ => "<unknown>"
      }
      Ast.Pattern.Unapply(className, pats.map(convertPattern))

    case T.Alternative(trees) =>
      Ast.Pattern.Alternative(trees.map(convertPattern))

    case T.Parens(pat) =>
      convertPattern(pat)

    case _ =>
      Ast.Pattern.Wildcard
  }

  /** Convert type tree to string representation */
  private def typeToString(tree: T.Tree): String = tree match {
    case T.Ident(name) => name.toString
    case T.Select(qual, name) => s"${typeToString(qual)}.${name}"
    case T.AppliedTypeTree(tpt, args) =>
      s"${typeToString(tpt)}[${args.map(typeToString).mkString(", ")}]"
    case _ => "<type>"
  }

  /** Convert for comprehension to method calls */
  private def convertForComprehension(enums: List[T.Tree], body: T.Tree, isYield: Boolean): Ast = {
    // Simplified: just convert to nested loops/maps
    enums match {
      case Nil =>
        convertExpr(body)

      case T.GenFrom(pat, rhs) :: rest =>
        val inner = convertForComprehension(rest, body, isYield)
        val patName = pat match {
          case T.Ident(n) => n.toString
          case T.Bind(n, _) => n.toString
          case _ => "_"
        }
        val methodName = if (isYield && rest.isEmpty) "map"
                         else if (isYield) "flatMap"
                         else "foreach"
        Ast.Apply(
          Ast.Select(convertExpr(rhs), methodName),
          List(Ast.Lambda(List(patName), inner))
        )

      case T.GenAlias(pat, rhs) :: rest =>
        val patName = pat match {
          case T.Ident(n) => n.toString
          case _ => "_"
        }
        val inner = convertForComprehension(rest, body, isYield)
        Ast.Block(
          List(Ast.ValDef(patName, convertExpr(rhs), false)),
          inner
        )

      case other :: rest =>
        // Guard or other - wrap in if
        val inner = convertForComprehension(rest, body, isYield)
        Ast.If(convertExpr(other), inner, Ast.UnitLit)
    }
  }
}

