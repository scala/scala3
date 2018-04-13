package scala.tasty
package util

import scala.runtime.tasty.Toolbox

class TreeTraverser(implicit toolbox: Toolbox) {

  def traverse(arg: statements.TopLevelStatement): Unit = {
    import statements._
    arg match {
      case arg: terms.Term =>
        traverse(arg)
      case arg: Definition =>
        traverse(arg)
      case Package(pkg, body) =>
        traverse(pkg)
        body.foreach(traverse)
      case Import(expr, _) =>
        traverse(expr)
      case _ =>
    }
  }

  def traverse(arg: statements.Definition): Unit = {
    import statements._
    arg match {
      case ValDef(_, tpt, rhs, _) =>
        traverse(tpt)
        rhs.foreach(traverse)
      case DefDef(_, typeParams, paramss, returnTpt, rhs, _) =>
        typeParams.foreach(traverse)
        paramss.foreach(_.foreach(traverse))
        traverse(returnTpt)
        rhs.foreach(traverse)
      case TypeDef(_, rhs, _) =>
        traverse(rhs)
      case ClassDef(_, constructor, parents, self, body, _) =>
        traverse(constructor)
        parents.foreach {
          case Left(term) => traverse(term)
          case Right(typetree) => traverse(typetree)
        }
        self.foreach(traverse)
        body.foreach(traverse)
      case _ =>
    }
  }

  def traverse(arg: terms.Term): Unit = {
    import terms._
    arg match {
      case Select(qual, _) =>
        traverse(qual)
      case New(tpt) =>
        traverse(tpt)
      case NamedArg(name, arg) =>
        traverse(arg)
      case Apply(fn, args) =>
        traverse(fn)
        args.foreach(traverse)
      case TypeApply(fn, args) =>
        traverse(fn)
        args.foreach(traverse)
      case Super(qual, mixin) =>
        traverse(qual)
      case Typed(expr, tpt) =>
        traverse(expr)
        traverse(tpt)
      case Assign(lhs, rhs) =>
        traverse(lhs)
        traverse(rhs)
      case Block(stats, expr) =>
        stats.foreach(x => traverse(x))
        traverse(expr)
      case Lambda(meth, tpt) =>
        traverse(meth)
        tpt.foreach(traverse)
      case If(cond, thenp, elsep) =>
        traverse(cond)
        traverse(thenp)
        traverse(elsep)
      case Match(selector, cases) =>
        traverse(selector)
        cases.foreach(traverse)
      case Try(body, catches, finalizer) =>
        traverse(body)
        catches.foreach(traverse)
        finalizer.foreach(traverse)
      case Return(expr) =>
        traverse(expr)
      case Repeated(args) =>
        args.foreach(traverse)
      case _ =>
      // Literal(const)
      // Ident(name)

    }
  }

  def traverse(arg: patterns.CaseDef): Unit = {
    import patterns.CaseDef
    arg match {
      case CaseDef(pat, guard, body) =>
        traverse(pat)
        guard.foreach(traverse)
        traverse(body)
      case _ =>
    }
  }

  def traverse(arg: patterns.Pattern): Unit = {
    import patterns._
    arg match {
      case Value(v) =>
        traverse(v)
      case Bind(_, body) =>
        traverse(body)
      case Unapply(fun, implicits, patterns) =>
        traverse(fun)
        implicits.foreach(traverse)
        patterns.foreach(traverse)
      case Alternative(patterns) =>
        patterns.foreach(traverse)
      case TypeTest(tpt) =>
        traverse(tpt)
      case _ =>
    }
  }

  def traverse(arg: typetrees.MaybeTypeTree): Unit = {
    import typetrees._
    arg match {
      case Select(qual, _) =>
        traverse(qual)
      case Singleton(ref) =>
        traverse(ref)
      case Refined(tpt, refinements) =>
        traverse(tpt)
        refinements.foreach(traverse)
      case Applied(tycon, args) =>
        traverse(tycon)
        args.foreach(traverse)
      case TypeBoundsTree(lo, hi) =>
        traverse(lo)
        traverse(hi)
      case Annotated(arg, annot) =>
        traverse(arg)
        traverse(annot)
      case And(left, right) =>
        traverse(left)
        traverse(right)
      case Or(left, right) =>
        traverse(left)
        traverse(right)
      case ByName(tpt) =>
        traverse(tpt)
      case _ =>
      // Ident(name)
      // Synthetic()
    }
  }
}
