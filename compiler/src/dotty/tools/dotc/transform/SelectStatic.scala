package dotty.tools.dotc
package transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.DenotTransformers.IdentityDenotTransformer
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core._
import dotty.tools.dotc.transform.MegaPhase._
import dotty.tools.dotc.transform.SymUtils._

/** Removes `Select`s that would be compiled into `GetStatic`.
 *
 *  Otherwise, the backend needs to be aware that some qualifiers need to be
 *  dropped.
 *
 *  A tranformation similar to what this phase does seems to be performed by
 *  flatten in nsc.
 *
 *  The side effects of the qualifier of a dropped `Select` is normally
 *  retained. As an exception, the qualifier is completely dropped if it is
 *  a reference to a static owner (see `isStaticOwnerRef`). Concretely, this
 *  means that in
 *
 *  {{{
 *  object Foo {
 *    println("side effects")
 *
 *    object Bar
 *    class Baz
 *  }
 *
 *  Foo.Bar
 *  new Foo.Baz()
 *  }}}
 *
 *  the `Foo` qualifiers will be dropped, since it is a static object. The
 *  `println("side effects")` will therefore not be executed.
 *
 *  This intended behavior is equivalent to what scalac does.
 *
 *  @author Dmytro Petrashko
 */
class SelectStatic extends MiniPhase with IdentityDenotTransformer {
  import ast.tpd._

  override def phaseName: String = SelectStatic.name

  override def description: String = SelectStatic.description

  override def transformSelect(tree: tpd.Select)(using Context): tpd.Tree = {
    val sym = tree.symbol
    def isStaticMember =
      (sym is Flags.Module) && sym.initial.maybeOwner.initial.isStaticOwner ||
      (sym is Flags.JavaStatic) ||
      sym.isScalaStatic
    val isStaticRef = !sym.is(Package) && !sym.maybeOwner.is(Package) && isStaticMember
    val tree1 =
      if isStaticRef && !tree.qualifier.symbol.isAllOf(JavaModule) && !tree.qualifier.isType then
        if isStaticOwnerRef(tree.qualifier) then ref(sym)
        else Block(List(tree.qualifier), ref(sym))
      else tree

    normalize(tree1)
  }

  private def isStaticOwnerRef(tree: Tree)(using Context): Boolean = tree match {
    case Ident(_) =>
      tree.symbol.is(Module) && tree.symbol.moduleClass.isStaticOwner
    case Select(qual, _) =>
      isStaticOwnerRef(qual) && tree.symbol.is(Module) && tree.symbol.moduleClass.isStaticOwner
    case _ =>
      false
  }

  private def normalize(t: Tree)(using Context) = t match {
    case Select(Block(stats, qual), nm) =>
      Block(stats, cpy.Select(t)(qual, nm))
    case Apply(Block(stats, qual), nm) =>
      Block(stats, Apply(qual, nm))
    case TypeApply(Block(stats, qual), nm) =>
      Block(stats, TypeApply(qual, nm))
    case Closure(env, Block(stats, qual), tpt) =>
      Block(stats, Closure(env, qual, tpt))
    case _ => t
  }

  override def transformApply(tree: tpd.Apply)(using Context): tpd.Tree =
    normalize(tree)

  override def transformTypeApply(tree: tpd.TypeApply)(using Context): tpd.Tree =
    normalize(tree)

  override def transformClosure(tree: tpd.Closure)(using Context): tpd.Tree =
    normalize(tree)
}

object SelectStatic:
  val name: String = "selectStatic"
  val description: String = "get rid of selects that would be compiled into GetStatic"
