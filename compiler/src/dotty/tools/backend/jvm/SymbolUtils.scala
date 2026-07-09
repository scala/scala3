package dotty.tools.backend.jvm

import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.*
import Contexts.*
import Symbols.*
import Phases.*
import SymDenotations.*
import NameKinds.{LazyBitMapName, LazyLocalName}
import dotty.tools.dotc.core.Names.TermName
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Types.MethodType

object SymbolUtils:
  def traitSuperAccessorName(sym: Symbol)(using Context): String =
    val nameString = sym.javaSimpleName
    if (sym.name == nme.TRAIT_CONSTRUCTOR) nameString
    else nameString + "$"

  def makeStatifiedDefSymbol(origSym: TermSymbol, name: TermName)(using Context): TermSymbol =
    val info = origSym.info match
      case mt: MethodType =>
        MethodType(nme.SELF :: mt.paramNames, origSym.owner.typeRef :: mt.paramInfos, mt.resType)
    origSym.copy(
      name = name.toTermName,
      flags = Method | JavaStatic,
      info = info
    ).asTerm

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
      private def isStaticModuleField(denot: SymDenotation)(using Context): Boolean =
        denot.owner.isStaticModuleClass && isField(denot) && !denot.name.is(LazyBitMapName) && !denot.name.is(LazyLocalName)

      def isStaticMember(using Context): Boolean =
        // guard against no symbol cause this code is executed to select which call type(static\dynamic) to use to call array.clone
        (sym ne NoSymbol) &&
        {
          val denot = sym.denot
          denot.is(JavaStatic) || sym.isScalaStatic || isStaticModuleField(denot)
        }

      /**
      * True for module classes of modules that are top-level or owned only by objects. Module classes
      * for such objects will get a MODULE$ flag and a corresponding static initializer.
      */
      def isStaticModuleClass(using Context): Boolean =
        val denot = sym.denot
        denot.is(Module) && {
          // scalac uses atPickling here
          // this would not work if modules are created after pickling
          // for example by specialization
          val original = denot.initial
          val validity = original.validFor
          atPhase(validity.lastPhaseId) {
            original.isStatic
          }
        }
      
      def originalLexicallyEnclosingClass(using Context): Symbol =
        // used to populate the EnclosingMethod attribute.
        // it is very tricky in presence of classes(and anonymous classes) defined inside supper calls.
        if (sym.exists) {
          val original = sym.denot.initial
          val validity = original.validFor
          atPhase(validity.lastPhaseId) {
            original.lexicallyEnclosingClass
          }
        } else NoSymbol

      /**
      * True for module classes of package level objects. The backend will generate a mirror class for
      * such objects.
      */
      def isTopLevelModuleClass(using Context): Boolean =
        sym.denot.is(ModuleClass) &&
        atPhase(flattenPhase) {
          sym.denot.owner.denot.is(PackageClass)
        }

      private def isField(denot: SymDenotation)(using Context): Boolean =
        denot.isTerm && !denot.isOneOf(Method | PhantomSymbol | NonMember | Package)
