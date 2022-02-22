package dotty.tools.backend.jvm

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.transform.SymUtils._
import java.io.{File => _}

import scala.reflect.ClassTag
import dotty.tools.io.AbstractFile
import dotty.tools.dotc.core._
import Contexts._
import Types._
import Symbols._
import Phases._

import dotty.tools.dotc.util.ReadOnlyMap
import dotty.tools.dotc.report

import tpd._

import StdNames.nme
import NameKinds.LazyBitMapName
import Names.Name

class DottyBackendInterface(val outputDirectory: AbstractFile, val superCallsMap: ReadOnlyMap[Symbol, Set[ClassSymbol]])(using val ctx: Context) {

  private val desugared = new java.util.IdentityHashMap[Type, tpd.Select]

  def cachedDesugarIdent(i: Ident): Option[tpd.Select] = {
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

  object DesugaredSelect extends DeconstructorCommon[tpd.Tree] {

    var desugared: tpd.Select = null

    override def isEmpty: Boolean =
      desugared eq null

    def _1: Tree =  desugared.qualifier

    def _2: Name = desugared.name

    override def unapply(s: tpd.Tree): this.type = {
      s match {
        case t: tpd.Select => desugared = t
        case t: Ident  =>
          cachedDesugarIdent(t) match {
            case Some(t) => desugared = t
            case None => desugared = null
          }
        case _ => desugared = null
      }

      this
    }
  }

  object ArrayValue extends DeconstructorCommon[tpd.JavaSeqLiteral] {
    def _1: Type = field.tpe match {
      case JavaArrayType(elem) => elem
      case _ =>
        report.error(s"JavaSeqArray with type ${field.tpe} reached backend: $field", ctx.source.atSpan(field.span))
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

}

object DottyBackendInterface {

  private def erasureString(clazz: Class[_]): String = {
    if (clazz.isArray) "Array[" + erasureString(clazz.getComponentType) + "]"
    else clazz.getName
  }

  def requiredClass(str: String)(using Context): ClassSymbol =
    Symbols.requiredClass(str)

  def requiredClass[T](using evidence: ClassTag[T], ctx: Context): Symbol =
    requiredClass(erasureString(evidence.runtimeClass))

  def requiredModule(str: String)(using Context): Symbol =
    Symbols.requiredModule(str)

  def requiredModule[T](using evidence: ClassTag[T], ctx: Context): Symbol = {
    val moduleName = erasureString(evidence.runtimeClass)
    val className = if (moduleName.endsWith("$")) moduleName.dropRight(1)  else moduleName
    requiredModule(className)
  }

  given symExtensions: AnyRef with
    extension (sym: Symbol)

      def isInterface(using Context): Boolean = (sym.is(PureInterface)) || sym.is(Trait)

      def isStaticConstructor(using Context): Boolean = (sym.isStaticMember && sym.isClassConstructor) || (sym.name eq nme.STATIC_CONSTRUCTOR)

      /** Fields of static modules will be static at backend
       *
       *  Note that lazy val encoding assumes bitmap fields are non-static.
       *  See also `genPlainClass` in `BCodeSkelBuilder.scala`.
       *
       *  TODO: remove the special handing of `LazyBitMapName` once we swtich to
       *        the new lazy val encoding: https://github.com/lampepfl/dotty/issues/7140
       */
      def isStaticModuleField(using Context): Boolean =
        sym.owner.isStaticModuleClass && sym.isField && !sym.name.is(LazyBitMapName)

      def isStaticMember(using Context): Boolean = (sym ne NoSymbol) &&
        (sym.is(JavaStatic) || sym.isScalaStatic || sym.isStaticModuleField)
        // guard against no sumbol cause this code is executed to select which call type(static\dynamic) to use to call array.clone

      /**
      * True for module classes of modules that are top-level or owned only by objects. Module classes
      * for such objects will get a MODULE$ flag and a corresponding static initializer.
      */
      def isStaticModuleClass(using Context): Boolean =
        (sym.is(Module)) && {
          // scalac uses atPickling here
          // this would not work if modules are created after pickling
          // for example by specialization
          val original = toDenot(sym).initial
          val validity = original.validFor
          atPhase(validity.phaseId) {
            toDenot(sym).isStatic
          }
        }



      def originalLexicallyEnclosingClass(using Context): Symbol =
        // used to populate the EnclosingMethod attribute.
        // it is very tricky in presence of classes(and annonymous classes) defined inside supper calls.
        if (sym.exists) {
          val validity = toDenot(sym).initial.validFor
          atPhase(validity.phaseId) {
            toDenot(sym).lexicallyEnclosingClass
          }
        } else NoSymbol

      /**
      * True for module classes of package level objects. The backend will generate a mirror class for
      * such objects.
      */
      def isTopLevelModuleClass(using Context): Boolean =
        sym.is(ModuleClass) &&
        atPhase(flattenPhase) {
          toDenot(sym).owner.is(PackageClass)
        }

      def javaSimpleName(using Context): String = toDenot(sym).name.mangledString
      def javaClassName(using Context): String = toDenot(sym).fullName.mangledString
      def javaBinaryName(using Context): String = javaClassName.replace('.', '/')

    end extension

  end symExtensions

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
  def isCompilingPrimitive(using Context) = {
    primitiveCompilationUnits(ctx.compilationUnit.source.file.name)
  }

}
