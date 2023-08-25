package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*, Flags.*
import Hashable.Binders
import printing.Showable
import util.SimpleIdentitySet
import Decorators.i
import scala.annotation.constructorOnly

type CaptureRoot = TermRef | CaptureRoot.Var

object CaptureRoot:

  case class Var(owner: Symbol, source: Symbol)(using @constructorOnly ictx: Context) extends CaptureRef, Showable:

    var upperBound: Symbol = owner
    var lowerBound: Symbol = NoSymbol
    var upperLevel: Int = owner.ccNestingLevel
    var lowerLevel: Int = Int.MinValue
    private[CaptureRoot] var lowerRoots: SimpleIdentitySet[Var] = SimpleIdentitySet.empty
    private[CaptureRoot] var upperRoots: SimpleIdentitySet[Var] = SimpleIdentitySet.empty
    private[CaptureRoot] var alias: CaptureRoot = this

    override def localRootOwner(using Context) = owner
    override def isTrackableRef(using Context): Boolean = true
    override def captureSetOfInfo(using Context) = CaptureSet.universal

    def setAlias(target: CaptureRoot) =
      alias = target

    def followAlias: CaptureRoot = alias match
      case alias: Var if alias ne this => alias.followAlias
      case _ => this

    def locallyConsistent =
      lowerLevel <= upperLevel
      && lowerRoots.forall(_.upperLevel <= upperLevel)
      && upperRoots.forall(_.lowerLevel >= lowerLevel)

    def computeHash(bs: Binders): Int = hash
    def hash: Int = System.identityHashCode(this)
    def underlying(using Context): Type = defn.Caps_Root.typeRef
  end Var

  def isEnclosingRoot(c1: CaptureRoot, c2: CaptureRoot)(using Context): Boolean =
    if c1 eq c2 then return true
    c1 match
      case c1: Var if c1.alias ne c1 => return isEnclosingRoot(c1.alias, c2)
      case _ =>
    c2 match
      case c2: Var if c2.alias ne c2 => return isEnclosingRoot(c1, c2.alias)
      case _ =>
    (c1, c2) match
      case (c1: TermRef, c2: TermRef) =>
        c1.ccNestingLevel <= c2.ccNestingLevel
      case (c1: TermRef, c2: Var) =>
        val level1 = c1.ccNestingLevel
        if level1 <= c2.lowerLevel then
          true // no change
        else if level1 <= c2.upperLevel && c2.upperRoots.forall(isEnclosingRoot(c1, _)) then
          if level1 == c2.upperLevel then
            c2.alias = c1
          else
            c2.lowerBound = c1.symbol
            c2.lowerLevel = level1
          true
        else false
      case (c1: Var, c2: TermRef) =>
        val level2 = c2.ccNestingLevel
        if c1.upperLevel <= level2 then
          true // no change
        else if c1.lowerLevel <= level2 && c1.lowerRoots.forall(isEnclosingRoot(_, c2)) then
          if level2 == c1.lowerLevel then
            c1.alias = c2
          else
            c1.upperBound = c2.symbol
            c1.upperLevel = level2
          true
        else false
      case (c1: Var, c2: Var) =>
        if c1.upperRoots.contains(c2) then
          true // no change
        else if c1.lowerLevel > c2.upperLevel then
          false // local inconsistency
        else
          c1.upperRoots += c2 // set early to prevent infinite looping
          if c1.lowerRoots.forall(isEnclosingRoot(_, c2))
            && c2.upperRoots.forall(isEnclosingRoot(c1, _))
          then
            if c1.lowerRoots.contains(c2) then
              val c2a = c2.followAlias
              if c2a ne c1 then c1.alias = c2a
            else
              if c1.upperLevel > c2.upperLevel then
                c1.upperBound = c2.upperBound
                c1.upperLevel = c2.upperLevel
              if c2.lowerLevel < c1.lowerLevel then
                c2.lowerBound = c1.lowerBound
                c2.lowerLevel = c1.lowerLevel
              c2.lowerRoots += c1
            true
          else
            c1.upperRoots -= c2
            false
  end isEnclosingRoot
end CaptureRoot

//class LevelError(val rvar: RootVar, val newBound: Symbol, val isUpper: Boolean) extends Exception



