package dotty.tools.backend.jvm


import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Flags.*

import java.io.{File => _}

import scala.reflect.ClassTag
import dotty.tools.io.AbstractFile
import dotty.tools.dotc.core.*
import Contexts.*
import Types.*
import Symbols.*
import Phases.*
import Decorators.em

import dotty.tools.dotc.util.ReadOnlyMap
import dotty.tools.dotc.report

import tpd.*

import StdNames.nme
import NameKinds.{LazyBitMapName, LazyLocalName}
import Names.Name

object DottyBackendInterface {

  private def erasureString(clazz: Class[?]): String = {
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

      def isInterface(using Context): Boolean = sym.is(PureInterface) || sym.is(Trait)

      def isStaticConstructor(using Context): Boolean = (sym.isStaticMember && sym.isClassConstructor) || (sym.name eq nme.STATIC_CONSTRUCTOR)

      /** Fields of static modules will be static at backend
       *
       *  Note that lazy val encoding assumes bitmap fields are non-static.
       *  See also `genPlainClass` in `BCodeSkelBuilder.scala`.
       *
       *  TODO: remove the special handing of `LazyBitMapName` once we swtich to
       *        the new lazy val encoding: https://github.com/scala/scala3/issues/7140
       */
      private def isStaticModuleField(using Context): Boolean =
        sym.owner.isStaticModuleClass && sym.isField && !sym.name.is(LazyBitMapName) && !sym.name.is(LazyLocalName)

      def isStaticMember(using Context): Boolean =
        // guard against no symbol cause this code is executed to select which call type(static\dynamic) to use to call array.clone
        (sym ne NoSymbol) &&
        (sym.is(JavaStatic) || sym.isScalaStatic || sym.isStaticModuleField)

      /**
      * True for module classes of modules that are top-level or owned only by objects. Module classes
      * for such objects will get a MODULE$ flag and a corresponding static initializer.
      */
      def isStaticModuleClass(using Context): Boolean =
        sym.is(Module) && {
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
        // it is very tricky in presence of classes(and anonymous classes) defined inside supper calls.
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
}
