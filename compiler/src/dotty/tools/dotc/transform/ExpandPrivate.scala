package dotty.tools.dotc
package transform

import core.*
import dotty.tools.dotc.core.DenotTransformers.IdentityDenotTransformer
import Contexts.*
import Symbols.*
import Flags.*
import SymDenotations.*

import Decorators.*
import MegaPhase.*
import java.io.File.separatorChar

import ValueClasses.*

/** Make private term members that are accessed from another class
 *  non-private by resetting the Private flag and expanding their name.
 *
 *  Make private accessor in value class not-private. This is necessary to unbox
 *  the value class when accessing it from separate compilation units
 *
 *  Make non-private any private parameter forwarders that forward to an inherited
 *  public or protected parameter accessor with the same name as the forwarder.
 *  This is necessary since private methods are not allowed to have the same name
 *  as inherited public ones.
 *
 *  Also, make non-private any private constructor that is annotated with `@publicInBinary`.
 *  (See SIP-52)
 *
 *  See discussion in https://github.com/scala/scala3/pull/784
 *  and https://github.com/scala/scala3/issues/783
 */
class ExpandPrivate extends MiniPhase with IdentityDenotTransformer { thisPhase =>
  import ast.tpd.*

  override def phaseName: String = ExpandPrivate.name

  override def description: String = ExpandPrivate.description

  // This phase moves methods around (in infotransform) so it may need to make other methods public
  override def runsAfter: Set[String] = Set(MoveStatics.name)

  override def changesMembers: Boolean = true // the phase introduces new members with mangled names

  override def checkPostCondition(tree: Tree)(using Context): Unit =
    tree match {
      case t: DefDef =>
        val sym = t.symbol
        def hasWeakerAccess(other: Symbol) =
          // public > protected > /* default */ > private
          if (sym.is(Private)) other.is(Private)
          else if (sym.is(Protected)) other.isOneOf(Protected | Private)
          else true // sym is public
        val fail = sym.allOverriddenSymbols.findSymbol(x => !hasWeakerAccess(x))
        if (fail.exists)
          assert(false, i"${sym.showFullName}: ${sym.info} has weaker access than superclass method ${fail.showFullName}: ${fail.info}")
      case _ =>
    }

  private def isVCPrivateParamAccessor(d: SymDenotation)(using Context) =
    d.isTerm && d.isAllOf(PrivateParamAccessor) && isDerivedValueClass(d.owner)

  /** Make private terms accessed from different classes non-private.
   *  Note: this happens also for accesses between class and linked module class.
   *  If we change the scheme at one point to make static module class computations
   *  static members of the companion class, we should tighten the condition below.
   */
  private def ensurePrivateAccessible(d: SymDenotation)(using Context) =
    if (isVCPrivateParamAccessor(d))
      d.ensureNotPrivate.installAfter(thisPhase)
    else if (d.is(PrivateTerm) && !d.owner.is(Package) && d.owner != ctx.owner.lexicallyEnclosingClass) {
      // Paths `p1` and `p2` are similar if they have a common suffix that follows
      // possibly different directory paths. That is, their common suffix extends
      // in both cases either to the start of the path or to a file separator character.
      // TODO: should we test absolute paths instead?
      def isSimilar(p1: String, p2: String): Boolean = {
        var i = p1.length - 1
        var j = p2.length - 1
        while (i >= 0 && j >= 0 && p1(i) == p2(j) && p1(i) != separatorChar) {
          i -= 1
          j -= 1
        }
        (i < 0 || p1(i) == separatorChar) &&
        (j < 0 || p2(j) == separatorChar)
      }

      // Skip assertion for @publicInBinary members - they are designed to be accessed
      // across compilation units (e.g., when inlined). See SIP-52.
      val isPublicInBinary = d.hasPublicInBinary
      assert(isPublicInBinary ||
             d.symbol.source.exists &&
             ctx.owner.source.exists &&
             isSimilar(d.symbol.source.path, ctx.owner.source.path),
          s"private ${d.symbol.showLocated} in ${d.symbol.source} accessed from ${ctx.owner.showLocated} in ${ctx.owner.source}")
      d.ensureNotPrivate.installAfter(thisPhase)
    }

  override def transformIdent(tree: Ident)(using Context): Ident = {
    ensurePrivateAccessible(tree.symbol)
    tree
  }

  override def transformSelect(tree: Select)(using Context): Select = {
    ensurePrivateAccessible(tree.symbol)
    tree
  }

  override def transformDefDef(tree: DefDef)(using Context): DefDef = {
    val sym = tree.symbol
    tree.rhs match {
      case _ if sym.isConstructor && sym.hasPublicInBinary =>
        sym.ensureNotPrivate.installAfter(thisPhase)
      case Apply(sel @ Select(_: Super, _), _)
      if sym.isAllOf(PrivateParamAccessor) && sel.symbol.is(ParamAccessor) && sym.name == sel.symbol.name =>
        sym.ensureNotPrivate.installAfter(thisPhase)
      case _ =>
        if (isVCPrivateParamAccessor(sym))
          sym.ensureNotPrivate.installAfter(thisPhase)
    }
    tree
  }
}

object ExpandPrivate:
  val name: String = "expandPrivate"
  val description: String = "widen private definitions accessed from nested classes"
