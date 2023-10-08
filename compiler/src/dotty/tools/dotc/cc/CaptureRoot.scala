package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*, Flags.*
import config.Printers.capt
import Hashable.Binders
import printing.Showable
import util.SimpleIdentitySet
import Decorators.*
import StdNames.nme
import scala.annotation.constructorOnly
import scala.annotation.internal.sharable

type CaptureRoot = TermRef | CaptureRoot.Var

object CaptureRoot:

  @sharable private var nextId = 0

  case class Var(owner: Symbol, source: Symbol)(using @constructorOnly ictx: Context) extends CaptureRef, Showable:

    val id =
      nextId += 1
      nextId

    var innerLimit: Symbol = owner.levelOwner
    var outerLimit: Symbol = defn.RootClass
    var outerRoots: SimpleIdentitySet[Var] = SimpleIdentitySet.empty

    override def isTrackableRef(using Context): Boolean = true
    override def captureSetOfInfo(using Context) = CaptureSet.universal

    private var myAlias: CaptureRoot = this
    def alias = myAlias
    def alias_=(r: CaptureRoot)(using Context) =
      //assert(id != 2, i"$this := $r")
      alias match
        case alias: TermRef =>
          val owner = alias.localRootOwner
          assert(
            owner.isContainedIn(outerLimit) && innerLimit.isContainedIn(owner),
            i"illegal alias $owner for $this")
        case _ =>
      myAlias = r

    /** A fresh var with the same limits and outerRoots as this one */
    def fresh(using Context): Var =
      val r = Var(owner, NoSymbol)
      r.innerLimit = innerLimit
      r.outerLimit = outerLimit
      r.outerRoots = outerRoots
      r

    /** A fresh var that is enclosed by all roots in `rs`.
     *  @throws A NoCommonRoot exception if this is not possible
     *          since root scopes dont' overlap.
     */
    def freshEnclosedBy(rs: CaptureRoot*)(using Context): CaptureRoot =
      val r = fresh
      if rs.forall(_.encloses(r)) then r else throw NoCommonRoot(rs*)

    def computeHash(bs: Binders): Int = hash
    def hash: Int = System.identityHashCode(this)
    def underlying(using Context): Type = defn.Caps_Cap.typeRef
  end Var

  class NoCommonRoot(rs: CaptureRoot*)(using Context) extends Exception(
    i"No common capture root nested in ${rs.mkString(" and ")}"
  )

  extension (r: CaptureRoot)

    def followAlias(using Context): CaptureRoot = r match
      case r: Var if r.alias ne r => r.alias.followAlias
      case _ => r

    def unifiesWith(other: CaptureRoot)(using Context): Boolean =
      r.encloses(other) && other.encloses(r)

    def encloses(other: CaptureRoot)(using Context): Boolean =
      val (r1, r2) = (followAlias, other.followAlias)
      (r1 eq r2) || (r1, r2).match
        case (r1: TermRef, r2: TermRef) =>
          r2.localRootOwner.isContainedIn(r1.localRootOwner)
        case (r1: TermRef, r2: Var) =>
          val r1Owner = r1.localRootOwner
          if r2.outerLimit.isContainedIn(r1Owner) then true
          else if !r2.innerLimit.isContainedIn(r1Owner) then false
          else
            if r2.innerLimit == r1Owner then r2.alias = r1
            else r2.outerLimit = r1Owner
            true
        case (r1: Var, r2: TermRef) =>
          val r2Owner = r2.localRootOwner
          if r2Owner.isContainedIn(r1.innerLimit) then true
          else if !r2Owner.isContainedIn(r1.outerLimit) then false
          else
            if r1.outerLimit == r2Owner then r1.alias = r2
            else r1.innerLimit = r2Owner
            true
        case (r1: Var, r2: Var) =>
          if r2.outerRoots.contains(r1) then true // no change
          else if !r2.innerLimit.isContainedIn(r1.outerLimit) then false // no overlap
          else if r1.outerRoots.contains(r2) then // unify
            r1.alias = r2
            r2.outerLimit =
              r1.outerLimit.maxNested(r2.outerLimit,
                onConflict = (_, _) => throw NoCommonRoot(r1, r2))
            r2.innerLimit = r1.innerLimit.minNested(r2.innerLimit)
            true
          else
            r2.outerRoots += r1 // set early to prevent infinite looping
            if r1.outerRoots.forall(_.encloses(r2)) then true
            else
              r2.outerRoots -= r2
              false
    end encloses
  end extension

  /** The capture root enclosed by `root1` and `root2`.
   *  If one of these is a Var, create a fresh Var with the appropriate constraints.
   *  If the scopes of `root1` and `root2` don't overlap, thow a `NoCommonRoot` exception.
   */
  def lub(root1: CaptureRoot, root2: CaptureRoot)(using Context): CaptureRoot =
    val (r1, r2) = (root1.followAlias, root2.followAlias)
    if r1 eq r2 then r1
    else (r1, r2) match
      case (r1: TermRef, r2: TermRef) =>
        r1.localRootOwner.maxNested(r2.localRootOwner,
          onConflict = (_, _) => throw NoCommonRoot(r1, r2)
        ).termRef
      case (r1: TermRef, r2: Var) =>
        r2.freshEnclosedBy(r1, r2)
      case (r1: Var, r2) =>
        r1.freshEnclosedBy(r1, r2)

end CaptureRoot



