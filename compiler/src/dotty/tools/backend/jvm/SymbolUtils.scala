package dotty.tools.backend.jvm

import dotty.tools.dotc.core.Flags.*

import dotty.tools.dotc.core.*
import Contexts.*
import Symbols.*
import Phases.*
import NameKinds.{LazyBitMapName, LazyLocalName}

object SymbolUtils:
  given symExtensions: AnyRef with
    extension (sym: Symbol)
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
          atPhase(validity.lastPhaseId) {
            toDenot(sym).isStatic
          }
        }
      
      def originalLexicallyEnclosingClass(using Context): Symbol =
        // used to populate the EnclosingMethod attribute.
        // it is very tricky in presence of classes(and anonymous classes) defined inside supper calls.
        if (sym.exists) {
          val validity = toDenot(sym).initial.validFor
          atPhase(validity.lastPhaseId) {
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
