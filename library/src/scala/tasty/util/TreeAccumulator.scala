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

  def foldTree(x: X, trees: Traversable[Tree])(implicit ctx: Context): X = (x /: trees)(foldTree)
  def foldTypeTree(x: X, trees: Traversable[MaybeTypeTree])(implicit ctx: Context): X = (x /: trees)(foldTypeTree)
  def foldCaseDef(x: X, trees: Traversable[CaseDef])(implicit ctx: Context): X = (x /: trees)(foldCaseDef)
  def foldPattern(x: X, trees: Traversable[Pattern])(implicit ctx: Context): X = (x /: trees)(foldPattern)
  def foldParent(x: X, trees: Traversable[Parent])(implicit ctx: Context): X = (x /: trees)(foldParent)

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
        foldTree(foldTree(x, fun), args)
      case TypeApply(fun, args) =>
        foldTypeTree(foldTree(x, fun), args)
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
        foldTree(foldTree(x, stats), expr)
      case If(cond, thenp, elsep) =>
        foldTree(foldTree(foldTree(x, cond), thenp), elsep)
      case Lambda(meth, tpt) =>
        val a = foldTree(x, meth)
        tpt.fold(a)(b => foldTypeTree(a, b))
      case Match(selector, cases) =>
        foldCaseDef(foldTree(x, selector), cases)
      case Return(expr) =>
        foldTree(x, expr)
      case Try(block, handler, finalizer) =>
        foldTree(foldCaseDef(foldTree(x, block), handler), finalizer)
      case Repeated(elems) =>
        foldTree(x, elems)
      case Inlined(call, bindings, expansion) =>
        foldTree(foldTree(x, bindings), expansion)

      case vdef @ ValDef(_, tpt, rhs) =>
        implicit val ctx = localCtx(vdef)
        foldTree(foldTypeTree(x, tpt), rhs)
      case ddef @ DefDef(_, tparams, vparamss, tpt, rhs) =>
        implicit val ctx = localCtx(ddef)
        foldTree(foldTypeTree((foldTree(x, tparams) /: vparamss)(foldTree), tpt), rhs)
      case tdef @ TypeDef(_, rhs) =>
        implicit val ctx = localCtx(tdef)
        foldTypeTree(x, rhs)
      case cdef @ ClassDef(_, constr, parents, self, body) =>
        implicit val ctx = localCtx(cdef)
        foldTree(foldTree(foldParent(foldTree(x, constr), parents), self), body)
      case Import(expr, selectors) =>
        foldTree(x, expr)
      case clause @ PackageClause(pid, stats) =>
        foldTree(foldTree(x, pid), stats)(localCtx(clause.definition))
    }
  }

  def foldOverTypeTree(x: X, tree: MaybeTypeTree)(implicit ctx: Context): X = tree match {
    case Synthetic() => x
    case TypeIdent(_) => x
    case TypeSelect(qualifier, _) => foldTree(x, qualifier)
    case Singleton(ref) => foldTree(x, ref)
    case And(left, right) => foldTypeTree(foldTypeTree(x, left), right)
    case Or(left, right) => foldTypeTree(foldTypeTree(x, left), right)
    case Refined(tpt, refinements) => foldTree(foldTypeTree(x, tpt), refinements)
    case Applied(tpt, args) => foldTypeTree(foldTypeTree(x, tpt), args)
    case ByName(result) => foldTypeTree(x, result)
    case Annotated(arg, annot) => foldTree(foldTypeTree(x, arg), annot)
    case TypeBoundsTree(lo, hi) => foldTypeTree(foldTypeTree(x, lo), hi)
  }

  def foldOverCaseDef(x: X, tree: CaseDef)(implicit ctx: Context): X = tree match {
    case CaseDef(pat, guard, body) => foldTree(foldTree(foldPattern(x, pat), guard), body)
  }

  def foldOverPattern(x: X, tree: Pattern)(implicit ctx: Context): X = tree match {
    case Value(v) => foldTree(x, v)
    case Bind(_, body) => foldPattern(x, body)
    case Unapply(fun, implicits, patterns) => foldPattern(foldTree(foldTree(x, fun), implicits), patterns)
    case Alternative(patterns) => foldPattern(x, patterns)
    case TypeTest(tpt) => foldTypeTree(x, tpt)
  }

  def foldOverParent(x: X, tree: Parent)(implicit ctx: Context): X = tree match {
    case TermParent(term) => foldOverTree(x, term)
    case TypeParent(typeTree) => foldOverTypeTree(x, typeTree)
  }

}
