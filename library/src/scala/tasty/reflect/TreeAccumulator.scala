package scala.tasty
package reflect

/** TASTy Reflect tree accumulator.
 *
 *  Usage:
 *  ```
 *  class MyTreeAccumulator[R <: scala.tasty.Reflection & Singleton](val reflect: R)
 *      extends scala.tasty.reflect.TreeAccumulator[X] {
 *    import reflect._
 *    def foldTree(x: X, tree: Tree)(using ctx: Context): X = ...
 *  }
 *  ```
 */
trait TreeAccumulator[X] {

  val reflect: Reflection
  import reflect._

  // Ties the knot of the traversal: call `foldOver(x, tree))` to dive in the `tree` node.
  def foldTree(x: X, tree: Tree)(using ctx: Context): X

  def foldTrees(x: X, trees: Iterable[Tree])(using ctx: Context): X = trees.foldLeft(x)(foldTree)

  def foldOverTree(x: X, tree: Tree)(using ctx: Context): X = {
    def localCtx(definition: Definition): Context = definition.symbol.localContext
    tree match {
      case Ident(_) =>
        x
      case Select(qualifier, _) =>
        foldTree(x, qualifier)
      case This(qual) =>
        x
      case Super(qual, _) =>
        foldTree(x, qual)
      case Apply(fun, args) =>
        foldTrees(foldTree(x, fun), args)
      case TypeApply(fun, args) =>
        foldTrees(foldTree(x, fun), args)
      case Literal(const) =>
        x
      case New(tpt) =>
        foldTree(x, tpt)
      case Typed(expr, tpt) =>
        foldTree(foldTree(x, expr), tpt)
      case NamedArg(_, arg) =>
        foldTree(x, arg)
      case Assign(lhs, rhs) =>
        foldTree(foldTree(x, lhs), rhs)
      case Block(stats, expr) =>
        foldTree(foldTrees(x, stats), expr)
      case If(cond, thenp, elsep) =>
        foldTree(foldTree(foldTree(x, cond), thenp), elsep)
      case While(cond, body) =>
        foldTree(foldTree(x, cond), body)
      case Closure(meth, tpt) =>
        foldTree(x, meth)
      case Match(selector, cases) =>
        foldTrees(foldTree(x, selector), cases)
      case Return(expr, _) =>
        foldTree(x, expr)
      case Try(block, handler, finalizer) =>
        foldTrees(foldTrees(foldTree(x, block), handler), finalizer)
      case Repeated(elems, elemtpt) =>
        foldTrees(foldTree(x, elemtpt), elems)
      case Inlined(call, bindings, expansion) =>
        foldTree(foldTrees(x, bindings), expansion)
      case vdef @ ValDef(_, tpt, rhs) =>
        val ctx = localCtx(vdef)
        given Context = ctx
        foldTrees(foldTree(x, tpt), rhs)
      case ddef @ DefDef(_, tparams, vparamss, tpt, rhs) =>
        val ctx = localCtx(ddef)
        given Context = ctx
        foldTrees(foldTree(vparamss.foldLeft(foldTrees(x, tparams))(foldTrees), tpt), rhs)
      case tdef @ TypeDef(_, rhs) =>
        val ctx = localCtx(tdef)
        given Context = ctx
        foldTree(x, rhs)
      case cdef @ ClassDef(_, constr, parents, derived, self, body) =>
        val ctx = localCtx(cdef)
        given Context = ctx
        foldTrees(foldTrees(foldTrees(foldTrees(foldTree(x, constr), parents), derived), self), body)
      case Import(expr, _) =>
        foldTree(x, expr)
      case clause @ PackageClause(pid, stats) =>
        foldTrees(foldTree(x, pid), stats)(using clause.symbol.localContext)
      case Inferred() => x
      case TypeIdent(_) => x
      case TypeSelect(qualifier, _) => foldTree(x, qualifier)
      case Projection(qualifier, _) => foldTree(x, qualifier)
      case Singleton(ref) => foldTree(x, ref)
      case Refined(tpt, refinements) => foldTrees(foldTree(x, tpt), refinements)
      case Applied(tpt, args) => foldTrees(foldTree(x, tpt), args)
      case ByName(result) => foldTree(x, result)
      case Annotated(arg, annot) => foldTree(foldTree(x, arg), annot)
      case LambdaTypeTree(typedefs, arg) => foldTree(foldTrees(x, typedefs), arg)
      case TypeBind(_, tbt) => foldTree(x, tbt)
      case TypeBlock(typedefs, tpt) => foldTree(foldTrees(x, typedefs), tpt)
      case MatchTypeTree(boundopt, selector, cases) =>
        foldTrees(foldTree(boundopt.fold(x)(foldTree(x, _)), selector), cases)
      case WildcardTypeTree() => x
      case TypeBoundsTree(lo, hi) => foldTree(foldTree(x, lo), hi)
      case CaseDef(pat, guard, body) => foldTree(foldTrees(foldTree(x, pat), guard), body)
      case TypeCaseDef(pat, body) => foldTree(foldTree(x, pat), body)
      case Bind(_, body) => foldTree(x, body)
      case Unapply(fun, implicits, patterns) => foldTrees(foldTrees(foldTree(x, fun), implicits), patterns)
      case Alternatives(patterns) => foldTrees(x, patterns)
    }
  }
}
