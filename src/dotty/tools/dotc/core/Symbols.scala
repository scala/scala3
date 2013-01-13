package dotty.tools.dotc
package core

import Periods._
import DenotationTransformers._
import Names._
import Flags._
import java.lang.AssertionError
import Decorators._
import Symbols._
import Contexts._
import Denotations._
import Types._
import References.{Reference, SymRef, UniqueSymRef, OverloadedRef}
import collection.mutable

object Symbols {


  /**
   * A SymRef is a period-dependent reference to a denotation.
   *  Given a period, its `deref` method resolves to a Symbol.
   */
  abstract class Symbol {

    def overriddenSymbol(inclass: ClassSymbol)(implicit ctx: Context): Symbol =
      if (owner isSubClass inclass) ???
      else NoSymbol

    def isProtected: Boolean = ???
    def isStable: Boolean = ???
    def accessBoundary: ClassSymbol = ???
    def isContainedIn(boundary: ClassSymbol) = ???
    def baseClasses: List[ClassSymbol] = ???
    def exists = true


    def orElse(that: => Symbol) = if (exists) this else that

    /** A isAbove B   iff  A can always be used instead of B
     */
    def isAbove(that: Symbol)(implicit ctx: Context): Boolean =
      (that.owner isSubClass this.owner) &&
      (this isAsAccessible that)

    /** A isBelow B   iff the reference A & B can always be simplified to A
     */
    def isBelow(that: Symbol)(implicit ctx: Context): Boolean =
      (this.owner isSubClass that.owner) ||
      (this isAsAccessible that)

    def isAsAccessible(that: Symbol)(implicit ctx: Context): Boolean =
      !this.isProtected && !that.isProtected && // protected members are incomparable
      (that.accessBoundary isContainedIn this.accessBoundary) &&
      this.isStable || !that.isStable


    /** Set the denotation of this symbol.
     */
    def setDenotation(denot: Denotation) =
      lastDenot = denot

    /** The last denotation of this symbol */
    protected[this] var lastDenot: Denotation = null

    /** Load denotation of this symbol */
    protected def loadDenot(implicit ctx: Context): Denotation

    /** The denotation of this symbol
     */
    def deref(implicit ctx: Context): Denotation = {
      val denot = lastDenot
      if (denot != null && (denot.validFor contains ctx.period))
        denot
      else
        trackedDenot
    }

    /** Get referenced denotation if lastDenot points to a different instance */
    private def trackedDenot(implicit ctx: Context): Denotation = {
      var denot = lastDenot
      if (denot == null) {
        denot = loadDenot
      } else {
        val currentPeriod = ctx.period
        val valid = denot.validFor
        val currentRunId = currentPeriod.runId
        val validRunId = valid.runId
        if (currentRunId != validRunId) {
          reloadDenot
        } else if (currentPeriod.code > valid.code) {
          // search for containing interval as long as nextInRun
          // increases.
          var nextDenot = denot.nextInRun
          while (nextDenot.validFor.code > valid.code && !(nextDenot.validFor contains currentPeriod)) {
            denot = nextDenot
            nextDenot = nextDenot.nextInRun
          }
          if (nextDenot.validFor.code > valid.code) {
            // in this case, containsPeriod(nextDenot.valid, currentPeriod)
            denot = nextDenot
          } else {
            // not found, denot points to highest existing variant
            var startPid = denot.validFor.lastPhaseId + 1
            val endPid = ctx.root.nextTransformer(startPid + 1).phaseId - 1
            nextDenot = ctx.root.nextTransformer(startPid) transform denot
            if (nextDenot eq denot)
              startPid = denot.validFor.firstPhaseId
            else {
              denot.nextInRun = nextDenot
              denot = nextDenot
            }
            denot.validFor = Period(currentRunId, startPid, endPid)
          }
        } else {
          // currentPeriod < valid; in this case a denotation must exist
          do {
            denot = denot.nextInRun
          } while (!(denot.validFor contains currentPeriod))
        }
      }
       denot
    }

    /**
     * Get loaded denotation if lastDenot points to a denotation from
     *  a different run.
     */
    private def reloadDenot(implicit ctx: Context): Denotation = {
      val initDenot = lastDenot.initial
      val newSym: Symbol =
        ctx.atPhase(FirstPhaseId) { implicit ctx =>
          initDenot.owner.info.decl(initDenot.name)
            .atSignature(thisRef.signature).symbol
        }
      if (newSym eq this) { // no change, change validity
        var d = initDenot
        do {
          d.validFor = Period(ctx.runId, d.validFor.firstPhaseId, d.validFor.lastPhaseId)
          d = d.nextInRun
        } while (d ne initDenot)
      }
      newSym.deref
    }

    def isType: Boolean
    def isTerm = !isType

    def thisRef(implicit ctx: Context): SymRef = new UniqueSymRef(this, info)

    // forwarders for sym methods
    def owner(implicit ctx: Context): Symbol = deref.owner
    def name(implicit ctx: Context): Name = deref.name
    def flags(implicit ctx: Context): FlagSet = deref.flags
    def info(implicit ctx: Context): Type = deref.info

    def prefix(implicit ctx: Context) = owner.thisType
    def allOverriddenSymbols: Iterator[Symbol] = ???
    def isAsAccessibleAs(other: Symbol): Boolean = ???
    def isAccessibleFrom(pre: Type)(implicit ctx: Context): Boolean = ???
    def locationString: String = ???
    def locatedFullString: String = ???
    def defString: String = ???
    def typeParams: List[TypeSymbol] = ???
    def unsafeTypeParams: List[TypeSymbol] = ???
    def thisType: Type = ???
    def isStaticMono = isStatic && typeParams.isEmpty
    def isPackageClass: Boolean = ???
    def isRoot: Boolean = ???
    def moduleClass: Symbol = ???
    def cloneSymbol: Symbol = ???

    def asTerm: TermSymbol = ???
    def asType: TypeSymbol = ???
    def asClass: ClassSymbol = ???
    def isStatic: Boolean = ???
    def isTypeParameter: Boolean = ???
    def isOverridable: Boolean = ???
    def isCovariant: Boolean = ???
    def isContravariant: Boolean = ???
    def isSkolem: Boolean = ???
    def isDeferred: Boolean = ???
    def isConcrete = !isDeferred
    def isJava: Boolean = ???

    def isSubClass(that: Symbol): Boolean = ???
    def isNonBottomSubClass(that: Symbol): Boolean = ???
    def isProperSubClass(that: Symbol): Boolean =
      (this ne that) && (this isSubClass that)

    def isAbstractType: Boolean = ???
    def newAbstractType(name: TypeName, info: TypeBounds): TypeSymbol = ???
    def newAbstractTerm(name: TermName, tpe: Type): TypeSymbol = ???

    def isClass: Boolean = false
    def isMethod(implicit ctx: Context): Boolean = deref.isMethod
    def hasFlag(required: FlagSet)(implicit ctx: Context): Boolean = (flags & required) != Flags.Empty
    def hasAllFlags(required: FlagSet)(implicit ctx: Context): Boolean = (flags & required) == flags

    def containsNull(implicit ctx: Context): Boolean =
      isClass && !(isSubClass(defn.AnyValClass))

  }

  abstract class TermSymbol extends Symbol {
    def name: TermName
    def isType = true
  }

  trait RefinementSymbol extends Symbol {
    override def deref(implicit ctx: Context) = lastDenot
  }

  abstract class RefinementTermSymbol extends TermSymbol with RefinementSymbol

  abstract class RefinementTypeSymbol extends TypeSymbol with RefinementSymbol

  abstract class TypeSymbol extends Symbol {
    def name: TypeName
    def isType = false

    def variance: Int = ???

    def typeConstructor(implicit ctx: Context): Type = ???
    def typeTemplate(implicit ctx: Context): Type = ???
  }

  abstract class ClassSymbol extends TypeSymbol {
    override def isClass = true
    private var superIdHint: Int = -1

    override def deref(implicit ctx: Context): ClassDenotation =
      super.deref.asInstanceOf[ClassDenotation]

    def typeOfThis(implicit ctx: Context): Type = ???

    override def typeConstructor(implicit ctx: Context): Type = deref.typeConstructor
    override def typeTemplate(implicit ctx: Context): Type = deref.typeTemplate

    /** The unique, densely packed identifier of this class symbol. Should be called
     *  only if class is a super class of some other class.
     */
    def superId(implicit ctx: Context): Int = {
      val hint = superIdHint
      val rctx = ctx.root
      if (hint >= 0 && hint <= rctx.lastSuperId && (rctx.classOfId(hint) eq this)) hint
      else {
        val id = rctx.superIdOfClass get this match {
          case Some(id) =>
            id
          case None =>
            val id = rctx.nextSuperId
            rctx.superIdOfClass(this) = id
            rctx.classOfId(id) = this
            id
        }
        superIdHint = id
        id
      }
    }
  }

  object NoSymbol extends Symbol {
    def loadDenot(implicit ctx: Context): Denotation = NoDenotation
    override def exists = false
    def isType = false
  }

  implicit def defn(implicit ctx: Context): Definitions = ctx.root.definitions
}