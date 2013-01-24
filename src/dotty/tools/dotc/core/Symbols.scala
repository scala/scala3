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
import reflect.io.AbstractFile

object Symbols {


  /** A Symbol represents a Scala definition/declaration or a package.
   */
  abstract class Symbol(denotf: Symbol => SymDenotation) {

     /** Is symbol different from NoSymbol? */
    def exists = true

    /** This symbol, if it exists, otherwise the result of evaluating `that` */
    def orElse(that: => Symbol) = if (exists) this else that

    def filter(p: Symbol => Boolean): Symbol = if (p(this)) this else NoSymbol

    /** The last denotation of this symbol */
    private[this] var lastDenot: SymDenotation = denotf(this)

    final def denot(implicit ctx: Context): SymDenotation = {
      var denot = lastDenot
      if (!(denot.validFor contains ctx.period)) denot = denot.current.asInstanceOf[SymDenotation]
      denot
    }

    def isType: Boolean = false
    def isTerm: Boolean = false
    def isClass: Boolean = false

    /** Special case tests for flags that are known a-priori and do not need loading
     *  flags.
     */
    def isModule(implicit ctx: Context) = denot.isModule
    def isModuleObj(implicit ctx: Context) = denot.isModuleObj
    def isModuleClass(implicit ctx: Context) = denot.isModuleClass
    def isPackage(implicit ctx: Context) = denot.isPackage
    def isPackageObj(implicit ctx: Context) = denot.isPackageObj
    def isPackageClass(implicit ctx: Context) = denot.isPackageClass

    /** A unique, densely packed integer tag for each class symbol, -1
     *  for all other symbols. To save memory, this method
     *  should be called only if class is a super class of some other class.
     */
    def superId: Int = -1

// --------- Forwarders for sym methods --------------------------

    /** The current owner of this symbol */
    final def owner(implicit ctx: Context): Symbol = denot.owner

    /** The current name of this symbol */
    final def name(implicit ctx: Context): Name = denot.name

    /** The current type info of this symbol */
    final def info(implicit ctx: Context): Type = denot.info

    /** The current flag set of this symbol */
    final def flags(implicit ctx: Context): FlagSet = denot.flags

    /** The current privateWithin boundary of this symbol, NoSymbol if no boundary is given. */
    final def privateWithin(implicit ctx: Context): Symbol = denot.privateWithin

    /** The current annotations of this symbol */
    final def annotations(implicit ctx: Context): List[Annotation] = denot.annotations

    /** Does this symbol have an annotation matching the given class symbol? */
    final def hasAnnotation(cls: Symbol)(implicit ctx: Context): Boolean = denot.hasAnnotation(cls)

    /** The chain of owners of this symbol, starting with the symbol itself */
    final def ownersIterator(implicit ctx: Context): Iterator[Symbol] = denot.ownersIterator

    /** Same as `ownersIterator contains sym` but more efficient. */
    final def hasTransOwner(sym: Symbol)(implicit ctx: Context): Boolean = denot.hasTransOwner(sym)

    /** The top-level class containing this symbol, except for a toplevel module
     *  its module class
     */
    def topLevelClass(implicit ctx: Context): Symbol = denot.topLevelClass

    /** The package containing this symbol */
    def enclosingPackage(implicit ctx: Context): Symbol = denot.enclosingPackage

    final def associatedFile(implicit ctx: Context): AbstractFile = denot.associatedFile
    final def binaryFile(implicit ctx: Context): AbstractFile = denot.binaryFile
    final def sourceFile(implicit ctx: Context): AbstractFile = denot.sourceFile

    final def companionClass(implicit ctx: Context): Symbol = denot.companionClass

    final def companionModule(implicit ctx: Context): Symbol = denot.companionModule

    final def linkedClass(implicit ctx: Context): Symbol = denot.linkedClass

    /** Is this symbol a subclass of the given class? */
    final def isSubClass(cls: Symbol)(implicit ctx: Context): Boolean = denot.isSubClass(cls)

   /** Is this class symbol a subclass of `cls`,
     * and is this class symbol also different from Null or Nothing?
     */
    final def isNonBottomSubClass(cls: Symbol)(implicit ctx: Context): Boolean = denot.isNonBottomSubClass(cls)

    /** Is this symbol a subclass of `base` or a companion object of such a subclass? */
    final def isSubClassOrCompanion(base: Symbol)(implicit ctx: Context): Boolean = denot.isSubClassOrCompanion(base)

    /** The class that encloses the owner of the current context
     *  and that is a subclass of this class.
     */
    final def enclosingSubClass(implicit ctx: Context) = denot.enclosingSubClass

    ///** Is this symbol a proper subclass of the given class? */
    //def isProperSubClass(cls: ClassSymbol)(implicit ctx: Context): Boolean = (this ne cls) && this.isSubClass(cls)

    /** The non-private symbol whose type matches the type of this symbol
     *  in in given class.
     *
     *  @param inClass   The class containing the symbol's definition
     *  @param site      The base type from which member types are computed
     */
    final def matchingSymbol(inClass: Symbol, site: Type)(implicit ctx: Context): Symbol = denot.matchingSymbol(inClass, site)

    /** The symbol, in class `inClass`, that is overridden by this symbol. */
    final def overriddenSymbol(inClass: ClassSymbol)(implicit ctx: Context): Symbol = denot.overriddenSymbol(inClass)

    /** All symbols overriden by this symbol. */
    final def allOverriddenSymbols(implicit ctx: Context): Iterator[Symbol] = denot.allOverriddenSymbols

    /** The class or term symbol up to which this symbol is accessible,
     *  or RootClass if it is public.  As java protected statics are
     *  otherwise completely inaccessible in scala, they are treated
     *  as public.
     *  @param base
     */
    final def accessBoundary(base: Symbol)(implicit ctx: Context): Symbol = denot.accessBoundary(base)

    /** Is this symbol contained in `boundary`? */
    final def isContainedIn(boundary: Symbol)(implicit ctx: Context): Boolean = denot.isContainedIn(boundary)

    /** Is this symbol accessible whenever `that` symbol is accessible?
     *  Does not take into account status of protected members.
     */
    final def isAsAccessibleAs(that: Symbol)(implicit ctx: Context): Boolean = denot.isAsAccessibleAs(that)

    /** Is this symbol a non-value class? */
    final def isNonValueClass(implicit ctx: Context): Boolean = denot.isNonValueClass

    /** Is this symbol accessible as a member of tree with type `pre`?
     *  @param pre          The type of the tree from which the selection is made
     *  @param superAccess  Access is via super
     */
    final def isAccessibleFrom(pre: Type, superAccess: Boolean = false)(implicit ctx: Context): Boolean = denot.isAccessibleFrom(pre, superAccess)

    def show(implicit ctx: Context): String = ctx.printer.show(this)
    def showLocated(implicit ctx: Context): String = ctx.printer.showLocated(this)
    def showDef(implicit ctx: Context): String = ctx.printer.showDef(this)

    def typeParams: List[TypeSymbol] = ???
    def unsafeTypeParams: List[TypeSymbol] = ???
    def thisType: Type = ???
    def isStaticMono = isStatic && typeParams.isEmpty
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

    def isAbstractType: Boolean = ???
    def newAbstractType(name: TypeName, info: TypeBounds): TypeSymbol = ???
    def newAbstractTerm(name: TermName, tpe: Type): TypeSymbol = ???

    //def isMethod(implicit ctx: Context): Boolean = denot.isMethod

    def isStable(implicit ctx: Context): Boolean = denot.isStable

  }

  abstract class TermSymbol(denotf: Symbol => SymDenotation) extends Symbol(denotf) {
    def name: TermName
    override def isTerm = true
  }

  abstract class TypeSymbol(denotf: Symbol => SymDenotation) extends Symbol(denotf) {
    def name: TypeName
    override def isType = true

    def variance: Int = ???

    def typeConstructor(implicit ctx: Context): Type = ???
    def typeTemplate(implicit ctx: Context): Type = ???
  }

  abstract class ClassSymbol(denotf: Symbol => ClassDenotation) extends TypeSymbol(denotf) {
    override def isClass = true
    private var superIdHint: Int = -1

    final def classDenot(implicit ctx: Context): ClassDenotation =
      denot.asInstanceOf[ClassDenotation]

    def typeOfThis(implicit ctx: Context): Type = ???

    def baseClasses(implicit ctx: Context): List[ClassSymbol] = classDenot.baseClasses

    override def typeConstructor(implicit ctx: Context): Type = classDenot.typeConstructor
    override def typeTemplate(implicit ctx: Context): Type = classDenot.typeTemplate

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

  class ErrorSymbol(underlying: Symbol, msg: => String)(implicit ctx: Context) extends Symbol(sym => underlying.denot) {
    override def isType = underlying.isType
    override def isTerm = underlying.isTerm
  }

  object NoSymbol extends Symbol(sym => NoDenotation) {
    override def exists = false
  }

  implicit def defn(implicit ctx: Context): Definitions = ctx.root.definitions

  implicit def toFlagSet(sym: Symbol)(implicit ctx: Context): FlagSet = sym.flags

}
