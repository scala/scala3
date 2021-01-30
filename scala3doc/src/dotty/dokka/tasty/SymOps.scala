package dotty.dokka.tasty

import dotty.dokka._
import dotty.dokka.model.api.Visibility
import dotty.dokka.model.api.VisibilityScope
import dotty.dokka.model.api.Modifier

import scala.quoted._

class SymOps[Q <: Quotes](val q: Q):
  import q.reflect._

  given Q = q
  extension (sym: Symbol)
    def packageName: String = (
      if (sym.isPackageDef) sym.fullName
      else sym.maybeOwner.packageName
    )

    def className: Option[String] =
      if (sym.isClassDef && !sym.flags.is(Flags.Package)) Some(
        Some(sym.maybeOwner).filter(s => s.exists).flatMap(_.className).fold("")(cn => cn + "$") + sym.name
      )
      else if (sym.isPackageDef) None
      else sym.maybeOwner.className

    def anchor: Option[String] =
      if (!sym.isClassDef && !sym.isPackageDef) Some(sym.name)
      else None
    //TODO: Retrieve string that will match scaladoc anchors

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

    def isCompanionObject: Boolean = sym.flags.is(Flags.Module) && sym.companionClass.exists

    def isGiven: Boolean = sym.flags.is(Flags.Given)

    def isExported: Boolean = sym.flags.is(Flags.Exported)

    def isOverriden: Boolean = sym.flags.is(Flags.Override)

    def isExtensionMethod: Boolean = sym.flags.is(Flags.ExtensionMethod)

    def isLeftAssoc(d: Symbol): Boolean = !d.name.endsWith(":")

    def extendedSymbol: Option[ValDef] =
      Option.when(sym.isExtensionMethod){
        val termParamss = sym.tree.asInstanceOf[DefDef].termParamss
        if isLeftAssoc(sym) || termParamss.size == 1 then termParamss(0).params(0)
        else termParamss(1).params(0)
      }

    // TODO #22 make sure that DRIs are unique plus probably reuse semantic db code?
    def dri: DRI =
      if sym == Symbol.noSymbol then topLevelDri
      else if sym.isValDef && sym.moduleClass.exists then sym.moduleClass.dri
      else
        val method =
          if (sym.isDefDef) Some(sym)
          else if (sym.maybeOwner.isDefDef) Some(sym.owner)
          else None

        val originPath = {
            import q.reflect._
            import dotty.tools.dotc
            given ctx: dotc.core.Contexts.Context = q.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
            val csym = sym.asInstanceOf[dotc.core.Symbols.Symbol]
            Option(csym.associatedFile).fold("")(_.path)
        }
        // We want package object to point to package
        val className = sym.className.filter(_ != "package$")

        DRI(
          className.fold(sym.packageName)(cn => s"${sym.packageName}.${cn}"),
          anchor = sym.anchor.getOrElse(""),
          origin = originPath,
          // sym.show returns the same signature for def << = 1 and def >> = 2.
          // For some reason it contains `$$$` instrad of symbol name
          s"${sym.name}${sym.fullName}/${sym.signature.resultSig}/[${sym.signature.paramSigs.mkString("/")}]$originPath"
        )
