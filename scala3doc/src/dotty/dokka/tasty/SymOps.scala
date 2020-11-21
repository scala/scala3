package dotty.dokka.tasty

import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model._
import collection.JavaConverters._
import dotty.dokka._
import dotty.dokka.model.api.Visibility
import dotty.dokka.model.api.VisibilityScope
import dotty.dokka.model.api.Modifier

import scala.quoted._

class SymOps[Q <: Quotes](val q: Q):
  import q.reflect._

  given Q = q
  extension (sym: Symbol):
    def packageName: String =
      if (sym.isPackageDef) sym.fullName
      else sym.maybeOwner.packageName

    def topLevelEntryName: Option[String] = if (sym.isPackageDef) None else
      if (sym.owner.isPackageDef) Some(sym.name) else sym.owner.topLevelEntryName

    def getVisibility(): Visibility =
      import VisibilityScope._

      def explicitScope(ownerType: TypeRepr): VisibilityScope =
        val moduleSym = ownerType.typeSymbol.companionModule
        if moduleSym.isNoSymbol
          then ExplicitTypeScope(ownerType.typeSymbol.name)
          else ExplicitModuleScope(moduleSym.name)

      def implicitScope(ownerSym: Symbol): VisibilityScope =
        val moduleSym = ownerSym.companionModule
        if moduleSym.isNoSymbol
          then ImplicitTypeScope
          else ImplicitModuleScope

      val visibilityFlags = (sym.flags.is(Flags.Private), sym.flags.is(Flags.Protected), sym.flags.is(Flags.Local))
      (sym.privateWithin, sym.protectedWithin, visibilityFlags) match
        case (Some(owner), None, _) => Visibility.Private(explicitScope(owner))
        case (None, Some(owner), _) => Visibility.Protected(explicitScope(owner))
        case (None, None, (true, false, _)) => Visibility.Private(implicitScope(sym.owner))
        case (None, None, (false, true, true)) => Visibility.Protected(ThisScope)
        case (None, None, (false, true, false)) => Visibility.Protected(implicitScope(sym.owner))
        case (None, None, (false, false, false)) => Visibility.Unrestricted
        case _ => throw new Exception(s"Visibility for symbol $sym cannot be determined")


    // Order here determines order in documenation
    def getExtraModifiers(): Seq[Modifier] = Seq(
        Flags.Final -> Modifier.Final,
        Flags.Sealed -> Modifier.Sealed,
        Flags.Erased -> Modifier.Erased,
        Flags.Abstract -> Modifier.Abstract,
        Flags.Implicit -> Modifier.Implicit,
        Flags.Inline -> Modifier.Inline,
        Flags.Lazy -> Modifier.Lazy,
        Flags.Open -> Modifier.Open,
        Flags.Override -> Modifier.Override,
        Flags.Case -> Modifier.Case,
        ).collect { case (flag, mod) if sym.flags.is(flag) => mod }

    def isHiddenByVisibility: Boolean =
      import VisibilityScope._

      getVisibility() match
        case Visibility.Private(_) => true
        case Visibility.Protected(ThisScope | ImplicitModuleScope | _: ExplicitModuleScope) => true
        case _ => false

    def shouldDocumentClasslike: Boolean = !isHiddenByVisibility
        && !sym.flags.is(Flags.Synthetic)
        && (!sym.flags.is(Flags.Case) || !sym.flags.is(Flags.Enum))
        && !(sym.companionModule.flags.is(Flags.Given))


    def getCompanionSymbol: Option[Symbol] = Some(sym.companionClass).filter(_.exists)

    def isCompanionObject: Boolean = sym.flags.is(Flags.Object) && sym.companionClass.exists

    def isGiven: Boolean = sym.flags.is(Flags.Given)

    def isExtensionMethod: Boolean = sym.flags.is(Flags.ExtensionMethod)

    def isLeftAssoc(d: Symbol): Boolean = !d.name.endsWith(":")

    def extendedSymbol: Option[ValDef] =
      Option.when(sym.isExtensionMethod)(
        if(isLeftAssoc(sym)) sym.tree.asInstanceOf[DefDef].paramss(0)(0)
        else sym.tree.asInstanceOf[DefDef].paramss(1)(0)
      )

    // TODO #22 make sure that DRIs are unique plus probably reuse semantic db code?
    def dri: DRI =
      if sym == Symbol.noSymbol then emptyDRI else if sym.isValDef && sym.moduleClass.exists then sym.moduleClass.dri else
        val pointsTo =
          if (!sym.isTypeDef) PointingToDeclaration.INSTANCE
          else PointingToGenericParameters(sym.owner.typeMembers.indexOf(sym))

        val method =
          if (sym.isDefDef) Some(sym)
          else if (sym.maybeOwner.isDefDef) Some(sym.owner)
          else None

        new DRI(
          sym.packageName,
          sym.topLevelEntryName.orNull, // TODO do we need any of this fields?
          method.map(s => new org.jetbrains.dokka.links.Callable(s.name, null, JList())).orNull,
          pointsTo, // TODO different targets?
          s"${sym.show}/${sym.signature.resultSig}/[${sym.signature.paramSigs.mkString("/")}]"
        )

  private val emptyDRI =  DRI.Companion.getTopLevel
