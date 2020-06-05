package dotty.tools.backend.jvm

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc
import dotty.tools.dotc.core.Flags.{termFlagSet}
import dotty.tools.dotc.transform.{Erasure, GenericSignatures}
import dotty.tools.dotc.transform.SymUtils._
import java.io.{File => _}

import scala.annotation.threadUnsafe
import scala.collection.generic.Clearable
import scala.collection.mutable
import scala.reflect.ClassTag
import dotty.tools.dotc.util.WeakHashSet
import dotty.tools.io.AbstractFile
import scala.tools.asm.AnnotationVisitor
import dotty.tools.dotc.core._
import Contexts._
import Types._
import Symbols._
import Phases._

import dotty.tools.dotc.util
import dotty.tools.dotc.util.Spans
import Decorators._
import Constants._
import tpd._

import scala.tools.asm
import StdNames.{nme, str}
import NameKinds.{DefaultGetterName, ExpandedName}
import Names.TermName
import Annotations.Annotation
import Names.Name

class DottyBackendInterface(val outputDirectory: AbstractFile, val superCallsMap: Map[Symbol, Set[ClassSymbol]])(implicit val ctx: Context) {
  import Symbols.{toDenot, toClassDenot}
    // Dotty deviation: Need to (re-)import implicit decorators here because otherwise
    // they would be shadowed by the more deeply nested `symHelper` decorator.


  type Symbol          = Symbols.Symbol
  type Type            = Types.Type

   // require LambdaMetafactory: scalac uses getClassIfDefined, but we need those always.
  @threadUnsafe lazy val LambdaMetaFactory: ClassSymbol = ctx.requiredClass("java.lang.invoke.LambdaMetafactory")
  @threadUnsafe lazy val MethodHandle: ClassSymbol      = ctx.requiredClass("java.lang.invoke.MethodHandle")

  val externalEquals: Symbol = defn.BoxesRunTimeModule.info.decl(nme.equals_).suchThat(toDenot(_).info.firstParamTypes.size == 2).symbol

  @threadUnsafe lazy val AnnotationRetentionAttr: ClassSymbol = ctx.requiredClass("java.lang.annotation.Retention")
  @threadUnsafe lazy val AnnotationRetentionSourceAttr: TermSymbol = ctx.requiredClass("java.lang.annotation.RetentionPolicy").linkedClass.requiredValue("SOURCE")
  @threadUnsafe lazy val AnnotationRetentionClassAttr: TermSymbol = ctx.requiredClass("java.lang.annotation.RetentionPolicy").linkedClass.requiredValue("CLASS")
  @threadUnsafe lazy val AnnotationRetentionRuntimeAttr: TermSymbol = ctx.requiredClass("java.lang.annotation.RetentionPolicy").linkedClass.requiredValue("RUNTIME")
  @threadUnsafe lazy val JavaAnnotationClass: ClassSymbol = ctx.requiredClass("java.lang.annotation.Annotation")

  val primitives = new DottyPrimitives(ctx)

  private def erasureString(clazz: Class[_]): String = {
    if (clazz.isArray) "Array[" + erasureString(clazz.getComponentType) + "]"
    else clazz.getName
  }

  def requiredClass[T](implicit evidence: ClassTag[T]): Symbol =
    ctx.requiredClass(erasureString(evidence.runtimeClass))

  def requiredModule[T](implicit evidence: ClassTag[T]): Symbol = {
    val moduleName = erasureString(evidence.runtimeClass)
    val className = if (moduleName.endsWith("$")) moduleName.dropRight(1)  else moduleName
    ctx.requiredModule(className)
  }

  def abort(msg: String): Nothing = {
    ctx.error(msg)
    throw new RuntimeException(msg)
  }
  def sourcePos(pos: Spans.Span)(implicit ctx: Context): util.SourcePosition =
    ctx.source.atSpan(pos)

  val perRunCaches: Caches = new Caches {
    def newAnyRefMap[K <: AnyRef, V](): mutable.AnyRefMap[K, V] = new mutable.AnyRefMap[K, V]()
    def newWeakMap[K, V](): mutable.WeakHashMap[K, V] = new mutable.WeakHashMap[K, V]()
    def recordCache[T <: Clearable](cache: T): T = cache
    def newWeakSet[K >: Null <: AnyRef](): WeakHashSet[K] = new WeakHashSet[K]()
    def newMap[K, V](): mutable.HashMap[K, V] = new mutable.HashMap[K, V]()
    def newSet[K](): mutable.Set[K] = new mutable.HashSet[K]
  }

  private val desugared = new java.util.IdentityHashMap[Type, tpd.Select]

  def desugarIdentBI(i: Ident): Option[tpd.Select] = {
    var found = desugared.get(i.tpe)
    if (found == null) {
      tpd.desugarIdent(i) match {
        case sel: tpd.Select =>
          desugared.put(i.tpe, sel)
          found = sel
        case _ =>
      }
    }
    if (found == null) None else Some(found)
  }

  // @M don't generate java generics sigs for (members of) implementation
  // classes, as they are monomorphic (TODO: ok?)
  private final def needsGenericSignature(sym: Symbol): Boolean = !(
    // pp: this condition used to include sym.hasexpandedname, but this leads
    // to the total loss of generic information if a private member is
    // accessed from a closure: both the field and the accessor were generated
    // without it.  This is particularly bad because the availability of
    // generic information could disappear as a consequence of a seemingly
    // unrelated change.
       ctx.base.settings.YnoGenericSig.value
    || sym.is(Flags.Artifact)
    || sym.isAllOf(Flags.LiftedMethod)
    || sym.is(Flags.Bridge)
  )

  private def verifySignature(sym: Symbol, sig: String)(implicit ctx: Context): Unit = {
    import scala.tools.asm.util.CheckClassAdapter
    def wrap(body: => Unit): Unit = {
      try body
      catch {
        case ex: Throwable =>
          ctx.error(i"""|compiler bug: created invalid generic signature for $sym in ${sym.denot.owner.showFullName}
                      |signature: $sig
                      |if this is reproducible, please report bug at https://github.com/lampepfl/dotty/issues
                   """.trim, sym.sourcePos)
          throw  ex
      }
    }

    wrap {
      if (sym.is(Flags.Method)) {
        CheckClassAdapter.checkMethodSignature(sig)
      }
      else if (sym.isTerm) {
        CheckClassAdapter.checkFieldSignature(sig)
      }
      else {
        CheckClassAdapter.checkClassSignature(sig)
      }
    }
  }

  /**
   * Generates the generic signature for `sym` before erasure.
   *
   * @param sym   The symbol for which to generate a signature.
   * @param owner The owner of `sym`.
   * @return The generic signature of `sym` before erasure, as specified in the Java Virtual
   *         Machine Specification, §4.3.4, or `null` if `sym` doesn't need a generic signature.
   * @see https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.3.4
   */
  def getGenericSignature(sym: Symbol, owner: Symbol): String = {
    ctx.atPhase(ctx.erasurePhase) {
      val memberTpe =
        if (sym.is(Flags.Method)) sym.denot.info
        else owner.denot.thisType.memberInfo(sym)
      getGenericSignature(sym, owner, memberTpe).orNull
    }
  }

  def getStaticForwarderGenericSignature(sym: Symbol, moduleClass: Symbol): String = {
    // scala/bug#3452 Static forwarder generation uses the same erased signature as the method if forwards to.
    // By rights, it should use the signature as-seen-from the module class, and add suitable
    // primitive and value-class boxing/unboxing.
    // But for now, just like we did in mixin, we just avoid writing a wrong generic signature
    // (one that doesn't erase to the actual signature). See run/t3452b for a test case.

    val memberTpe = ctx.atPhase(ctx.erasurePhase) { moduleClass.denot.thisType.memberInfo(sym) }
    val erasedMemberType = TypeErasure.erasure(memberTpe)
    if (erasedMemberType =:= sym.denot.info)
      getGenericSignature(sym, moduleClass, memberTpe).orNull
    else null
  }

  private def getGenericSignature(sym: Symbol, owner: Symbol, memberTpe: Type)(implicit ctx: Context): Option[String] =
    if (needsGenericSignature(sym)) {
      val erasedTypeSym = TypeErasure.fullErasure(sym.denot.info).typeSymbol
      if (erasedTypeSym.isPrimitiveValueClass) {
        // Suppress signatures for symbols whose types erase in the end to primitive
        // value types. This is needed to fix #7416.
        None
      } else {
        val jsOpt = GenericSignatures.javaSig(sym, memberTpe)
        if (ctx.settings.XverifySignatures.value) {
          jsOpt.foreach(verifySignature(sym, _))
        }

        jsOpt
      }
    } else {
      None
    }


  def assocsFromApply(tree: Tree): List[(Name, Tree)] = {
    tree match {
      case Block(_, expr) => assocsFromApply(expr)
      case Apply(fun, args) =>
        fun.tpe.widen match {
          case MethodType(names) =>
            (names zip args).filter {
              case (_, t: tpd.Ident) if (t.tpe.normalizedPrefix eq NoPrefix) => false
              case _ => true
            }
        }
    }
  }

  def symHelper(sym: Symbol): SymbolHelper = new SymbolHelper(sym)

  class SymbolHelper(sym: Symbol) {

    // tests
    def isPublic: Boolean = !sym.flags.isOneOf(Flags.Private | Flags.Protected)
    def isInterface: Boolean = (sym.is(Flags.PureInterface)) || sym.is(Flags.Trait)

    /** Does this symbol actually correspond to an interface that will be emitted?
     *  In the backend, this should be preferred over `isInterface` because it
     *  also returns true for the symbols of the fake companion objects we
     *  create for Java-defined classes as well as for Java annotations
     *  which we represent as classes.
     */
    def isEmittedInterface: Boolean = isInterface ||
      sym.is(Flags.JavaDefined) && (toDenot(sym).isAnnotation || sym.is(Flags.ModuleClass) && (sym.companionClass.is(Flags.PureInterface)) || sym.companionClass.is(Flags.Trait))

    def isStaticMember: Boolean = (sym ne NoSymbol) &&
      (sym.is(Flags.JavaStatic) || sym.hasAnnotation(ctx.definitions.ScalaStaticAnnot))
      // guard against no sumbol cause this code is executed to select which call type(static\dynamic) to use to call array.clone

    /**
     * True for module classes of modules that are top-level or owned only by objects. Module classes
     * for such objects will get a MODULE$ flag and a corresponding static initializer.
     */
    def isStaticModuleClass: Boolean =
      (sym.is(Flags.Module)) && {
        // scalac uses atPickling here
        // this would not work if modules are created after pickling
        // for example by specialization
        val original = toDenot(sym).initial
        val validity = original.validFor
        val shiftedContext = ctx.withPhase(validity.phaseId)
        toDenot(sym)(shiftedContext).isStatic(shiftedContext)
      }

    def isStaticConstructor: Boolean = (isStaticMember && sym.isClassConstructor) || (sym.name eq nme.STATIC_CONSTRUCTOR)


    // navigation

    def originalLexicallyEnclosingClass: Symbol =
      // used to populate the EnclosingMethod attribute.
      // it is very tricky in presence of classes(and annonymous classes) defined inside supper calls.
      if (sym.exists) {
        val validity = toDenot(sym).initial.validFor
        val shiftedContext = ctx.withPhase(validity.phaseId)
        toDenot(sym)(shiftedContext).lexicallyEnclosingClass(shiftedContext)
      } else NoSymbol

    // members

    /** For currently compiled classes: All locally defined classes including local classes.
     *  The empty list for classes that are not currently compiled.

     */
    def nestedClasses: List[Symbol] = definedClasses(ctx.flattenPhase)

    /** For currently compiled classes: All classes that are declared as members of this class
     *  (but not inherited ones). The empty list for classes that are not currently compiled.
     */
    def memberClasses: List[Symbol] = definedClasses(ctx.lambdaLiftPhase)

    private def definedClasses(phase: Phase) =
      if (sym.isDefinedInCurrentRun)
        ctx.atPhase(phase) {
          toDenot(sym).info.decls.filter(_.isClass)
        }
      else Nil

    /**
     * True for module classes of package level objects. The backend will generate a mirror class for
     * such objects.
     */
    def isTopLevelModuleClass: Boolean = sym.is(Flags.ModuleClass) &&
      ctx.atPhase(ctx.flattenPhase) {
        toDenot(sym).owner.is(Flags.PackageClass)
      }


    def samMethod(): Symbol = ctx.atPhase(ctx.erasurePhase) {
      val samMethods = toDenot(sym).info.possibleSamMethods.toList
      samMethods match {
        case x :: Nil => x.symbol
        case Nil => abort(s"${sym.show} is not a functional interface. It doesn't have abstract methods")
        case xs => abort(s"${sym.show} is not a functional interface. " +
          s"It has the following abstract methods: ${xs.map(_.name).mkString(", ")}")
      }
    }

  }

  object SelectBI extends DeconstructorCommon[tpd.Tree] {

    var desugared: tpd.Select = null

    override def isEmpty: Boolean =
      desugared eq null

    def _1: Tree =  desugared.qualifier

    def _2: Name = desugared.name

    override def unapply(s: tpd.Tree): this.type = {
      s match {
        case t: tpd.Select => desugared = t
        case t: Ident  =>
          desugarIdentBI(t) match {
            case Some(t) => desugared = t
            case None => desugared = null
          }
        case _ => desugared = null
      }

      this
    }
  }

  object ThrowBI {
    var field: tpd.Apply = _
    def isEmpty: Boolean = field eq null
    def isDefined = !isEmpty
    def get: Tree = field.args.head
    def unapply(s: tpd.Apply): ThrowBI.type = {
      if (s.fun.symbol eq defn.throwMethod) {
        field = s
      } else {
        field = null
      }
      this
    }
  }

  object ArrayValueBI extends DeconstructorCommon[tpd.JavaSeqLiteral] {
    def _1: Type = field.tpe match {
      case JavaArrayType(elem) => elem
      case _ =>
        ctx.error(s"JavaSeqArray with type ${field.tpe} reached backend: $field", sourcePos(field.span))
        UnspecifiedErrorType
    }
    def _2: List[Tree] = field.elems
  }



  abstract class DeconstructorCommon[T >: Null <: AnyRef] {
    var field: T = null
    def get: this.type = this
    def isEmpty: Boolean = field eq null
    def isDefined = !isEmpty
    def unapply(s: T): this.type ={
      field = s
      this
    }
  }

  abstract class Caches {
    def recordCache[T <: Clearable](cache: T): T
    def newWeakMap[K, V](): collection.mutable.WeakHashMap[K, V]
    def newMap[K, V](): collection.mutable.HashMap[K, V]
    def newSet[K](): collection.mutable.Set[K]
    def newWeakSet[K >: Null <: AnyRef](): dotty.tools.dotc.util.WeakHashSet[K]
    def newAnyRefMap[K <: AnyRef, V](): collection.mutable.AnyRefMap[K, V]
  }

  // Class symbols used in backend.
  // Vals because they are to frequent in scala programs so that they are already loaded by backend

  lazy val NativeAttr: Symbol = requiredClass[scala.native]
  lazy val TransientAttr = requiredClass[scala.transient]
  lazy val VolatileAttr = requiredClass[scala.volatile]

  val ScalaATTRName: String = "Scala"
  val ScalaSignatureATTRName: String = "ScalaSig"

  // Module symbols used in backend
  val StringModule: Symbol = requiredClass[java.lang.String].linkedClass
  val ScalaRunTimeModule: Symbol = requiredModule[scala.runtime.ScalaRunTime.type]


  private val primitiveCompilationUnits = Set(
    "Unit.scala",
    "Boolean.scala",
    "Char.scala",
    "Byte.scala",
    "Short.scala",
    "Int.scala",
    "Float.scala",
    "Long.scala",
    "Double.scala"
  )

  /**
   * True if the current compilation unit is of a primitive class (scala.Boolean et al).
   * Used only in assertions.
   */
  def isCompilingPrimitive = {
    primitiveCompilationUnits(ctx.compilationUnit.source.file.name)
  }

}

object DottyBackendInterface {
  val ExcludedForwarderFlags: Flags.FlagSet = {
    Flags.Specialized | Flags.Lifted | Flags.Protected | Flags.JavaStatic |
    Flags.Private | Flags.Macro
  }
}
