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

  extension symExtensions on (sym: Symbol) {

    def isInterface: Boolean = (sym.is(Flags.PureInterface)) || sym.is(Flags.Trait)

    def isStaticConstructor: Boolean = (sym.isStaticMember && sym.isClassConstructor) || (sym.name eq nme.STATIC_CONSTRUCTOR)

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



    def originalLexicallyEnclosingClass: Symbol =
      // used to populate the EnclosingMethod attribute.
      // it is very tricky in presence of classes(and annonymous classes) defined inside supper calls.
      if (sym.exists) {
        val validity = toDenot(sym).initial.validFor
        val shiftedContext = ctx.withPhase(validity.phaseId)
        toDenot(sym)(shiftedContext).lexicallyEnclosingClass(shiftedContext)
      } else NoSymbol

    /**
     * True for module classes of package level objects. The backend will generate a mirror class for
     * such objects.
     */
    def isTopLevelModuleClass: Boolean =
      sym.is(Flags.ModuleClass) &&
      ctx.atPhase(ctx.flattenPhase) {
        toDenot(sym).owner.is(Flags.PackageClass)
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
        ctx.error(s"JavaSeqArray with type ${field.tpe} reached backend: $field", ctx.source.atSpan(field.span))
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
