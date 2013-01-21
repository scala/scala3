package dotty.tools.dotc
package core

import Periods._
import Transformers._
import Names._
import Flags._
import java.lang.AssertionError
import Decorators._
import Symbols._
import Contexts._
import SymDenotations._
import Types._, Annotations._
import Denotations.{Denotation, SingleDenotation, MultiDenotation}
import collection.mutable

object Symbols {


  /** A Symbol represents a Scala definition/declaration or a package.
   */
  abstract class Symbol {

     /** Is symbol different from NoSymbol? */
    def exists = true

    /** This symbol, if it exists, otherwise the result of evaluating `that` */
    def orElse(that: => Symbol) = if (exists) this else that

    /** Set the denotation of this symbol.
     */
    def setDenotation(denot: SymDenotation) =
      lastDenot = denot

    /** The last denotation of this symbol */
    protected[this] var lastDenot: SymDenotation = null

    /** Load denotation of this symbol */
    protected def loadDenot(implicit ctx: Context): SymDenotation

    /** The denotation of this symbol
     */
    def denot(implicit ctx: Context): SymDenotation = {
      val denot = lastDenot
      if (denot == null) loadDenot
      else {
        val currentPeriod = ctx.period
        val valid = denot.validFor
        if (valid contains currentPeriod) denot
        else if (valid.runId != currentPeriod.runId) reloadDenot
        else denot.current.asSymDenotation
      }
    }

    /**
     * Get loaded denotation if lastDenot points to a denotation from
     *  a different run. !!! needed?
     */
    private def reloadDenot(implicit ctx: Context): SymDenotation = {
      val initDenot = lastDenot.initial.asSymDenotation
      val newSym: Symbol =
        ctx.atPhase(FirstPhaseId) { implicit ctx =>
          initDenot.owner.info.decl(initDenot.name)
            .atSignature(denot.signature).symbol
        }
      if (newSym eq this) { // no change, change validity
        var d = initDenot
        do {
          d.validFor = Period(ctx.runId, d.validFor.firstPhaseId, d.validFor.lastPhaseId)
          d = d.nextInRun.asSymDenotation
        } while (d ne initDenot)
      }
      newSym.denot
    }

    def isType: Boolean
    def isTerm = !isType



    // forwarders for sym methods
    def owner(implicit ctx: Context): Symbol = denot.owner
    def name(implicit ctx: Context): Name = denot.name
    def flags(implicit ctx: Context): FlagSet = denot.flags
    def info(implicit ctx: Context): Type = denot.info

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
    def hasAnnotation(ann: Annotation): Boolean = ???
    def hasAnnotation(ann: ClassSymbol): Boolean = ???


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

    def privateWithin(implicit ctx: Context): Symbol = denot.privateWithin
    def isAbstractType: Boolean = ???
    def newAbstractType(name: TypeName, info: TypeBounds): TypeSymbol = ???
    def newAbstractTerm(name: TermName, tpe: Type): TypeSymbol = ???

    def isClass: Boolean = false
    def isMethod(implicit ctx: Context): Boolean = denot.isMethod
    def hasFlag(required: FlagSet)(implicit ctx: Context): Boolean = (flags & required) != Flags.Empty
    def hasAllFlags(required: FlagSet)(implicit ctx: Context): Boolean = (flags & required) == flags

    /** The non-private symbol whose type matches the type of this symbol
     *  in in given class.
     *
     *  @param ofClass   The class containing the symbol's definition
     *  @param site      The base type from which member types are computed
     */
    final def matchingSymbol(inClass: Symbol, site: Type)(implicit ctx: Context): Symbol = {
      var ref = inClass.info.nonPrivateDecl(name)
      if (ref.isTerm) {
        val targetType = site.memberInfo(this)
        if (ref.isOverloaded) ref = ref.atSignature(targetType.signature)
        val candidate = ref.symbol
        if (site.memberInfo(candidate) matches targetType) candidate
        else NoSymbol
      } else ref.symbol
    }

    def overriddenSymbol(inClass: ClassSymbol)(implicit ctx: Context): Symbol =
      if (owner isSubClass inClass) matchingSymbol(inClass, owner.thisType)
      else NoSymbol

    def isStable(implicit ctx: Context): Boolean = false

    /** The class or term symbol up to which this symbol is accessible,
     *  or RootClass if it is public.  As java protected statics are
     *  otherwise completely inaccessible in scala, they are treated
     *  as public.
     *  @param base
     */
    def accessBoundary(base: Symbol)(implicit ctx: Context): Symbol = {
      val denot = this.denot
      val fs = denot.flags
      if (fs is PrivateOrLocal) owner
      else if (fs is StaticProtected) defn.RootClass
      else if (denot.privateWithin.exists && !ctx.phase.erasedTypes) denot.privateWithin
      else if (fs is Protected) base
      else defn.RootClass
    }

    /** Is this symbol contained in `boundary`? */
    def isContainedIn(boundary: Symbol)(implicit ctx: Context): Boolean =
      if (this eq boundary) true
      else if (!this.exists ||
               (this is PackageClass) && !(boundary is PackageClass)) false
      else owner.isContainedIn(boundary)

   /** Is this symbol accessible whenever `that` symbol is accessible?
    *  Does not take into account status of protected members.
    */
   def isAsAccessible(that: Symbol)(implicit ctx: Context): Boolean =
     (that.accessBoundary(NoSymbol) isContainedIn this.accessBoundary(NoSymbol)) &&
     this.isStable || !that.isStable

    def containsNull(implicit ctx: Context): Boolean =
      isClass && !(isSubClass(defn.AnyValClass))
  }

  abstract class TermSymbol extends Symbol {
    def name: TermName
    def isType = true

    override def isStable(implicit ctx: Context) = !(
      this.is(UnstableValue, butNot = Stable) ||
      info.isVolatile && !hasAnnotation(defn.uncheckedStableClass)
    )
  }

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

    override def denot(implicit ctx: Context): ClassDenotation =
      super.denot.asInstanceOf[ClassDenotation]

    def typeOfThis(implicit ctx: Context): Type = ???

    def baseClasses(implicit ctx: Context): List[ClassSymbol] = denot.baseClasses

    override def typeConstructor(implicit ctx: Context): Type = denot.typeConstructor
    override def typeTemplate(implicit ctx: Context): Type = denot.typeTemplate

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
    def loadDenot(implicit ctx: Context): SymDenotation = NoDenotation
    override def exists = false
    def isType = false
    override def isTerm = false
  }

  implicit def defn(implicit ctx: Context): Definitions = ctx.root.definitions

  implicit def toFlagSet(sym: Symbol)(implicit ctx: Context): FlagSet = sym.flags

}
