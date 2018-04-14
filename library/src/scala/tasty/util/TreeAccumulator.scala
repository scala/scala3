package scala.tasty.util

import scala.runtime.tasty.Toolbox

import scala.tasty._

abstract class TreeAccumulator[X](implicit toolbox: Toolbox) {
  // Ties the knot of the traversal: call `foldOver(x, tree))` to dive in the `tree` node.
  def apply(x: X, tree: statements.TopLevelStatement): X
  def apply(x: X, tree: typetrees.MaybeTypeTree): X
  def apply(x: X, tree: patterns.CaseDef): X
  def apply(x: X, tree: patterns.Pattern): X

  def apply(x: X, tree: scala.util.Either[terms.Term, typetrees.TypeTree]): X = tree match {
    case Left(term) => apply(x, term)
    case Right(typetree) => apply(x, typetree)
  }

  def apply(x: X, trees: Traversable[statements.TopLevelStatement]): X = (x /: trees)(apply)
  def apply1(x: X, trees: Traversable[typetrees.MaybeTypeTree]): X = (x /: trees)(apply)
  def apply2(x: X, trees: Traversable[scala.util.Either[terms.Term, typetrees.TypeTree]]): X = (x /: trees)(apply)
  def apply3(x: X, trees: Traversable[patterns.CaseDef]): X = (x /: trees)(apply)
  def apply4(x: X, trees: Traversable[patterns.Pattern]): X = (x /: trees)(apply)

  def foldOver(x: X, tree: statements.TopLevelStatement): X = {
    import statements._
    import terms._
    tree match {
      case Ident(name) =>
        x
      case Select(qualifier, name) =>
        this(x, qualifier)
      case This(qual) =>
        x
      case Super(qual, mix) =>
        this(x, qual)
      case Apply(fun, args) =>
        this(this(x, fun), args)
      case TypeApply(fun, args) =>
        apply1(this(x, fun), args)
      case Literal(const) =>
        x
      case New(tpt) =>
        this(x, tpt)
      case Typed(expr, tpt) =>
        this(this(x, expr), tpt)
      case NamedArg(name, arg) =>
        this(x, arg)
      case Assign(lhs, rhs) =>
        this(this(x, lhs), rhs)
      case Block(stats, expr) =>
        this(this(x, stats), expr)
      case If(cond, thenp, elsep) =>
        this(this(this(x, cond), thenp), elsep)
      case Lambda(meth, tpt) =>
        val a = this(x, meth)
        tpt.fold(a)(b => this(a, b))
      case Match(selector, cases) =>
        apply3(this(x, selector), cases)
      case Return(expr) =>
        this(x, expr)
      case Try(block, handler, finalizer) =>
        this(apply3(this(x, block), handler), finalizer)
      case Repeated(elems) =>
        this(x, elems)
      case Inlined(call, bindings, expansion) =>
        this(this(x, bindings), expansion)
      case ValDef(_, tpt, rhs, _) =>
        this(this(x, tpt), rhs)
      case DefDef(_, tparams, vparamss, tpt, rhs, _) =>
        this(this((this(x, tparams) /: vparamss)(apply), tpt), rhs)
      case TypeDef(name, rhs, _) =>
        this(x, rhs)
      case ClassDef(_, constr, parents, self, body, _) =>
        this(this(apply2(this(x, constr), parents), self), body)
      case Import(expr, selectors) =>
        this(x, expr)
      case Package(pid, stats) =>
        this(this(x, pid), stats)
      case _ =>
        x
    }
  }

  def foldOver(x: X, tree: patterns.CaseDef): X = {
    import patterns._
    tree match {
      case CaseDef(pat, guard, body) =>
        this(this(this(x, pat), guard), body)
      case _ =>
        x
    }
  }

  def foldOver(x: X, tree: patterns.Pattern): X = {
    import patterns._
    tree match {
      case Value(v) =>
        this(x, v)
      case Bind(_, body) =>
        this(x, body)
      case Unapply(fun, implicits, patterns) =>
        apply4(this(this(x, fun), implicits), patterns)
      case Alternative(patterns) =>
        apply4(x, patterns)
      case TypeTest(tpt) =>
        this(x, tpt)
      case _ =>
        x
    }
  }

  def foldOver(x: X, tree: typetrees.MaybeTypeTree): X = {
    import typetrees._
    tree match {
      case Ident(name) =>
        x
      case Select(qualifier, name) =>
        this(x, qualifier)
      case Singleton(ref) =>
        this(x, ref)
      case And(left, right) =>
        this(this(x, left), right)
      case Or(left, right) =>
        this(this(x, left), right)
      case Refined(tpt, refinements) =>
        this(this(x, tpt), refinements)
      case Applied(tpt, args) =>
        apply1(this(x, tpt), args)
      case ByName(result) =>
        this(x, result)
      case TypeBoundsTree(lo, hi) =>
        this(this(x, lo), hi)
      case Annotated(arg, annot) =>
        this(this(x, arg), annot)
      case _ =>
        x
    }
  }
}
