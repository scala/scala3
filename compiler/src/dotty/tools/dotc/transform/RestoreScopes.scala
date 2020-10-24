package dotty.tools
package dotc
package transform

import core._
import DenotTransformers.IdentityDenotTransformer
import Contexts._
import Symbols._
import Scopes._
import MegaPhase.MiniPhase
import ast.Trees._
import StdNames._

/** The preceding lambda lift and flatten phases move symbols to different scopes
 *  and rename them. This miniphase cleans up afterwards and makes sure that all
 *  class scopes contain the symbols defined in them.
 */
class RestoreScopes extends MiniPhase with IdentityDenotTransformer { thisPhase =>
  import ast.tpd._
  override def phaseName: String = "restoreScopes"

  override def changesMembers: Boolean = true // the phase affects scopes, applying tree transformations of previous phases

  /* Note: We need to wait until we see a package definition because
   * DropEmptyConstructors changes template members when analyzing the
   * enclosing package definitions. So by the time RestoreScopes gets to
   * see a typedef or template, it still might be changed by DropEmptyConstructors.
   */
  override def transformPackageDef(pdef: PackageDef)(using Context): PackageDef = {
    pdef.stats.foreach(restoreScope)
    pdef
  }

  private def restoreScope(tree: Tree)(using Context) = tree match {
    case TypeDef(_, impl: Template) =>
      val restoredDecls = newScope
      def restore(stat: Tree) =
        if stat.isInstanceOf[MemberDef] && stat.symbol.exists then
          restoredDecls.enter(stat.symbol)
      restore(impl.constr)
      impl.body.foreach(restore)
      // Enter class in enclosing package scope, in case it was an inner class before flatten.
      // For top-level classes this does nothing.
      val cls = tree.symbol.asClass
      val pkg = cls.owner.asClass

      pkg.enter(cls)
      tree.symbol.copySymDenotation(
        info =  cls.classInfo.derivedClassInfo(
          decls = restoredDecls: Scope)).installAfter(thisPhase)
      tree
    case tree => tree
  }
}

