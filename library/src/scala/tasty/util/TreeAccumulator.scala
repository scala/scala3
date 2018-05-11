package scala.tasty.util

import scala.tasty.Tasty

abstract class TreeAccumulator[X, T <: Tasty with Singleton](val tasty: T) {
  import tasty._

  // Ties the knot of the traversal: call `foldOver(x, tree))` to dive in the `tree` node.
  def foldTree(x: X, tree: Tree)(implicit ctx: Context): X
  def foldTypeTree(x: X, tree: MaybeTypeTree)(implicit ctx: Context): X
  def foldCaseDef(x: X, tree: CaseDef)(implicit ctx: Context): X
  def foldPattern(x: X, tree: Pattern)(implicit ctx: Context): X
  def foldParent(x: X, tree: Parent)(implicit ctx: Context): X

  def foldTrees(x: X, trees: Iterable[Tree])(implicit ctx: Context): X = (x /: trees)(foldTree)
  def foldTypeTrees(x: X, trees: Iterable[MaybeTypeTree])(implicit ctx: Context): X = (x /: trees)(foldTypeTree)
  def foldCaseDefs(x: X, trees: Iterable[CaseDef])(implicit ctx: Context): X = (x /: trees)(foldCaseDef)
  def foldPatterns(x: X, trees: Iterable[Pattern])(implicit ctx: Context): X = (x /: trees)(foldPattern)
  def foldParents(x: X, trees: Iterable[Parent])(implicit ctx: Context): X = (x /: trees)(foldParent)

  def foldOverTree(x: X, tree: Tree)(implicit ctx: Context): X = {
    def localCtx(definition: Definition): Context = definition.localContext
    tree match {
      case Ident(_) =>
        x
      case Select(qualifier, _, _) =>
        foldTree(x, qualifier)
      case This(qual) =>
        x
      case Super(qual, _) =>
        foldTree(x, qual)
      case Apply(fun, args) =>
        foldTrees(foldTree(x, fun), args)
      case TypeApply(fun, args) =>
        foldTypeTrees(foldTree(x, fun), args)
      case Literal(const) =>
        x
      case New(tpt) =>
        foldTypeTree(x, tpt)
      case Typed(expr, tpt) =>
        foldTypeTree(foldTree(x, expr), tpt)
      case NamedArg(_, arg) =>
        foldTree(x, arg)
      case Assign(lhs, rhs) =>
        foldTree(foldTree(x, lhs), rhs)
      case Block(stats, expr) =>
        foldTree(foldTrees(x, stats), expr)
      case If(cond, thenp, elsep) =>
        foldTree(foldTree(foldTree(x, cond), thenp), elsep)
      case Lambda(meth, tpt) =>
        val a = foldTree(x, meth)
        tpt.fold(a)(b => foldTypeTree(a, b))
      case Match(selector, cases) =>
        foldCaseDefs(foldTree(x, selector), cases)
      case Return(expr) =>
        foldTree(x, expr)
      case Try(block, handler, finalizer) =>
        foldTrees(foldCaseDefs(foldTree(x, block), handler), finalizer)
      case Repeated(elems) =>
        foldTrees(x, elems)
      case Inlined(call, bindings, expansion) =>
        foldTree(foldTrees(x, bindings), expansion)

      case vdef @ ValDef(_, tpt, rhs) =>
        implicit val ctx = localCtx(vdef)
        foldTrees(foldTypeTree(x, tpt), rhs)
      case ddef @ DefDef(_, tparams, vparamss, tpt, rhs) =>
        implicit val ctx = localCtx(ddef)
        foldTrees(foldTypeTree((foldTrees(x, tparams) /: vparamss)(foldTrees), tpt), rhs)
      case tdef @ TypeDef(_, rhs) =>
        implicit val ctx = localCtx(tdef)
        foldTypeTree(x, rhs)
      case cdef @ ClassDef(_, constr, parents, self, body) =>
        implicit val ctx = localCtx(cdef)
        foldTrees(foldTrees(foldParents(foldTree(x, constr), parents), self), body)
      case Import(expr, selectors) =>
        foldTree(x, expr)
      case clause @ PackageClause(pid, stats) =>
        foldTrees(foldTree(x, pid), stats)(localCtx(clause.definition))
    }
  }

  def foldOverTypeTree(x: X, tree: MaybeTypeTree)(implicit ctx: Context): X = tree match {
    case Synthetic() => x
    case TypeIdent(_) => x
    case TypeSelect(qualifier, _) => foldTree(x, qualifier)
    case Singleton(ref) => foldTree(x, ref)
    case And(left, right) => foldTypeTree(foldTypeTree(x, left), right)
    case Or(left, right) => foldTypeTree(foldTypeTree(x, left), right)
    case Refined(tpt, refinements) => foldTrees(foldTypeTree(x, tpt), refinements)
    case Applied(tpt, args) => foldTypeTrees(foldTypeTree(x, tpt), args)
    case ByName(result) => foldTypeTree(x, result)
    case Annotated(arg, annot) => foldTree(foldTypeTree(x, arg), annot)
    case TypeBoundsTree(lo, hi) => foldTypeTree(foldTypeTree(x, lo), hi)
  }

  def foldOverCaseDef(x: X, tree: CaseDef)(implicit ctx: Context): X = tree match {
    case CaseDef(pat, guard, body) => foldTree(foldTrees(foldPattern(x, pat), guard), body)
  }

  def foldOverPattern(x: X, tree: Pattern)(implicit ctx: Context): X = tree match {
    case Value(v) => foldTree(x, v)
    case Bind(_, body) => foldPattern(x, body)
    case Unapply(fun, implicits, patterns) => foldPatterns(foldTrees(foldTree(x, fun), implicits), patterns)
    case Alternative(patterns) => foldPatterns(x, patterns)
    case TypeTest(tpt) => foldTypeTree(x, tpt)
  }

  def foldOverParent(x: X, tree: Parent)(implicit ctx: Context): X = tree match {
    case TermParent(term) => foldOverTree(x, term)
    case TypeParent(typeTree) => foldOverTypeTree(x, typeTree)
  }

}
