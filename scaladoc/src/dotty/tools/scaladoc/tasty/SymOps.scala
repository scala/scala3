package dotty.tools.scaladoc
package tasty

import scala.quoted._
import dotty.tools.scaladoc.util.Escape._
import scala.collection.mutable.{ Map => MMap }
import dotty.tools.io.AbstractFile
import Scaladoc2AnchorCreator.getScaladoc2Type
import JavadocAnchorCreator.getJavadocType

object SymOps:

  extension (using Quotes)(sym: reflect.Symbol)

    def isImplicitClass: Boolean =
      import reflect._
      sym.isClassDef && sym.maybeOwner != Symbol.noSymbol
        && sym.maybeOwner.declaredMethods.exists { methodSymbol =>
          methodSymbol.name == sym.name && methodSymbol.flags.is(Flags.Implicit) && methodSymbol.flags.is(Flags.Method)
        }

    def packageName: String =
      if (sym.isPackageDef) sym.fullName
      else sym.maybeOwner.packageName

    def packageNameSplitted: Seq[String] =
      sym.packageName.split('.').toList

    def className: Option[String] =
      import reflect._
      if (sym.isClassDef && !sym.flags.is(Flags.Package)) Some(
        Some(sym.maybeOwner).filter(s => s.exists).flatMap(_.className).fold("")(cn => cn + "$") + sym.name
      ).filterNot(_.contains("package$"))
      else if (sym.isPackageDef) None
      else sym.maybeOwner.className

    def anchor: Option[String] =
      if (!sym.isClassDef && !sym.isPackageDef) {
        val params = sym.signature.paramSigs.map {
          case s: String => s
          case i: Int => i.toString
        }
        val result = sym.signature.resultSig
        val hash = ((params.mkString + result).hashCode % 4096).toHexString
        Some(s"${sym.name}-$hash")
      }
      else None

    def source =
      val path = sym.pos.flatMap(_.sourceFile.getJPath).map(_.toAbsolutePath)
      path.map(TastyMemberSource(_, sym.pos.get.startLine))

    //TODO: Retrieve string that will match scaladoc anchors


    def getVisibility(): Visibility =
      import reflect._
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
        case (None, None, (true, true, false)) => Visibility.Protected(ThisScope)
        case _ => throw new Exception(s"Visibility for symbol $sym cannot be determined")


    // Order here determines order in documenation
    def getExtraModifiers(): Seq[Modifier] =
      import reflect._
      Seq(
        Flags.Final -> Modifier.Final,
        Flags.Sealed -> Modifier.Sealed,
        Flags.Erased -> Modifier.Erased,
        Flags.Abstract -> Modifier.Abstract,
        Flags.Deferred -> Modifier.Deferred,
        Flags.Implicit -> Modifier.Implicit,
        Flags.Infix -> Modifier.Infix,
        Flags.Transparent -> Modifier.Transparent,
        Flags.Inline -> Modifier.Inline,
        Flags.Lazy -> Modifier.Lazy,
        Flags.Open -> Modifier.Open,
        Flags.Override -> Modifier.Override,
        Flags.Case -> Modifier.Case,
      ).collect {
        case (flag, mod) if sym.flags.is(flag) => mod
      }

    def isHiddenByVisibility(using dctx: DocContext): Boolean =
      import VisibilityScope._

      !summon[DocContext].args.includePrivateAPI && sym.getVisibility().match
        case Visibility.Private(_) => true
        case Visibility.Protected(ThisScope | ImplicitModuleScope | _: ExplicitModuleScope) => true
        case _ => false

    def shouldDocumentClasslike(using dctx: DocContext): Boolean =
      import reflect._
      !sym.isHiddenByVisibility
      && !sym.flags.is(Flags.Synthetic)
      && (!sym.flags.is(Flags.Case) || !sym.flags.is(Flags.Enum))

    def getCompanionSymbol: Option[reflect.Symbol] = Some(sym.companionClass).filter(_.exists)

    def isCompanionObject: Boolean =
      import reflect._
      sym.flags.is(Flags.Module) && sym.companionClass.exists

    def isGiven: Boolean =
      import reflect._
      sym.flags.is(Flags.Given)

    def isExported: Boolean =
      import reflect._
      sym.flags.is(Flags.Exported)

    def isOverridden: Boolean =
      import reflect._
      sym.flags.is(Flags.Override)

    def isExtensionMethod: Boolean =
      import reflect._
      sym.flags.is(Flags.ExtensionMethod)

    def isArtifact: Boolean =
      import reflect._
      sym.flags.is(Flags.Artifact)

    def isLeftAssoc: Boolean = !sym.name.endsWith(":")

    def extendedSymbol: Option[reflect.ValDef] =
      import reflect.*
      if sym.isExtensionMethod then
        sym.extendedTermParamLists.find(param => !param.isImplicit && !param.isGiven).flatMap(_.params.headOption)
      else None

    def splitExtensionParamList: (List[reflect.ParamClause], List[reflect.ParamClause]) =
      import reflect.*
      val method = sym.tree.asInstanceOf[DefDef]
      (for {
        defPosition <- method.symbol.pos
        defStart <- scala.util.Try(defPosition.start).toOption
      } yield {
        method.paramss.partition(_.params.headOption.flatMap(_.symbol.pos.map(_.start < defStart)).getOrElse(false))
      }).getOrElse(List.empty, List.empty)

    def extendedTypeParams: List[reflect.TypeDef] =
      import reflect.*
      val method = sym.tree.asInstanceOf[DefDef]
      method.leadingTypeParams

    def extendedTermParamLists: List[reflect.TermParamClause] =
      import reflect.*
      sym.splitExtensionParamList._1.collect { 
        case tpc: TermParamClause => tpc 
      }

    def nonExtensionTermParamLists: List[reflect.TermParamClause] =
      import reflect.*
      if sym.nonExtensionLeadingTypeParams.nonEmpty then
        sym.nonExtensionParamLists.dropWhile {
          case _: TypeParamClause => false
          case _ => true
        }.drop(1).collect {
          case tpc: TermParamClause => tpc
        }
      else
        sym.nonExtensionParamLists.collect {
          case tpc: TermParamClause => tpc
        }

    def nonExtensionParamLists: List[reflect.ParamClause] =
      sym.splitExtensionParamList._2

    def nonExtensionLeadingTypeParams: List[reflect.TypeDef] =
      import reflect.*
      sym.nonExtensionParamLists.collectFirst {
        case TypeParamClause(params) => params
      }.toList.flatten

  end extension

end SymOps

// TODO find a better way to handle this cache and move the methods to SymOps
class SymOpsWithLinkCache:
  import SymOps.*

  private val externalLinkCache: scala.collection.mutable.Map[AbstractFile, Option[ExternalDocLink]] = MMap()

  extension (using Quotes)(sym: reflect.Symbol)

    private def constructPath(location: Seq[String], anchor: Option[String], link: ExternalDocLink): String =
      import reflect.*
      val extension = ".html"
      val docURL = link.documentationUrl.toString
      def constructPathForJavadoc: String =
        val l = "\\$+".r.replaceAllIn(location.mkString("/"), _ => ".")
        val javadocAnchor = if anchor.isDefined then {
          val paramSigs = sym.paramSymss.flatten.map(_.tree).collect {
            case v: ValDef => v.tpt.tpe
          }.map(getJavadocType)
          "#" + sym.name + paramSigs.mkString("-","-","-")
        } else ""
        docURL + l + extension + javadocAnchor

      //TODO #263: Add anchor support
      def constructPathForScaladoc2: String =
        val l = escapeUrl(location.mkString("/"))
        val scaladoc2Anchor = if anchor.isDefined then {
          "#" + getScaladoc2Type(sym.tree)
        } else ""
        docURL + l + extension + scaladoc2Anchor

      // TODO Add tests for it!
      def constructPathForScaladoc3: String =
        val base = docURL + escapeUrl(location.mkString("/")) + extension
        anchor.fold(base)(a => base + "#" + a)

      link.kind match {
        case DocumentationKind.Javadoc => constructPathForJavadoc
        case DocumentationKind.Scaladoc2 => constructPathForScaladoc2
        case DocumentationKind.Scaladoc3 => constructPathForScaladoc3
      }

    // TODO #22 make sure that DRIs are unique plus probably reuse semantic db code?
    def dri(using dctx: DocContext): DRI =
      import reflect.*
      if sym == Symbol.noSymbol then topLevelDri
      else
        val method =
          if (sym.isDefDef) Some(sym)
          else if (sym.maybeOwner.isDefDef) Some(sym.owner)
          else None

        val (className, anchor) = if sym.fullName == "scala.AnyRef" then // hacking relocation for synthetic `type AnyRef`
          (Some("AnyRef"), None)
        else
          (sym.className, sym.anchor)

        val location = (sym.packageNameSplitted ++ className).map(escapeFilename(_))

        val externalLink = {
            import reflect._
            import dotty.tools.dotc
            given ctx: dotc.core.Contexts.Context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
            val csym = sym.asInstanceOf[dotc.core.Symbols.Symbol]
            val extLink = if externalLinkCache.contains(csym.associatedFile)
              then externalLinkCache(csym.associatedFile)
              else {
                def calculatePath(file: AbstractFile): String = file.underlyingSource.filter(_ != file).fold("")(f => calculatePath(f) + "/") + file.path
                val calculatedLink = Option(csym.associatedFile).map(f => calculatePath(f)).flatMap { path =>
                  dctx.externalDocumentationLinks.find(_.originRegexes.exists(r => r.matches(path)))
                }
                externalLinkCache += (csym.associatedFile -> calculatedLink)
                calculatedLink
              }
            extLink.map(link => sym.constructPath(location, anchor, link))
        }

        DRI(
          location.mkString("."),
          anchor.getOrElse(""),
          externalLink = externalLink,
          // sym.show returns the same signature for def << = 1 and def >> = 2.
          // For some reason it contains `$$$` instrad of symbol name
          s"${sym.name}${sym.fullName}/${sym.signature.resultSig}/[${sym.signature.paramSigs.mkString("/")}]"
        )

    def driInContextOfInheritingParent(par: reflect.Symbol)(using dctx: DocContext): DRI = sym.dri.copy(
      location = par.dri.location,
      externalLink = None
    )
