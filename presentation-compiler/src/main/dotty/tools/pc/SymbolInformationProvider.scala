package dotty.tools.pc

import scala.collection.mutable
import scala.meta.internal.pc.PcSymbolInformation
import scala.meta.internal.pc.SymbolInfo
import scala.meta.pc.PcSymbolKind
import scala.meta.pc.PcSymbolProperty
import scala.util.control.NonFatal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Denotations.{Denotation, MultiDenotation}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.pc.SemanticdbSymbols
import dotty.tools.pc.utils.InteractiveEnrichments.allSymbols
import dotty.tools.pc.utils.InteractiveEnrichments.deepDealiasAndSimplify
import dotty.tools.pc.utils.InteractiveEnrichments.stripBackticks

class SymbolInformationProvider(using Context):

  def info(symbol: String): Option[PcSymbolInformation] =
    val foundSymbols = SymbolProvider.compilerSymbols(symbol)

    val (searchedSymbol, alternativeSymbols) =
      foundSymbols.partition(compilerSymbol =>
        SemanticdbSymbols.symbolName(compilerSymbol) == symbol
      )

    searchedSymbol match
      case Nil => None
      case sym :: _ =>
        val classSym = if sym.isClass then sym else sym.moduleClass
        val parents =
          if classSym.isClass then
            val classInfo = classSym.asClass
            val parentSymbols = classInfo.parentSyms

            val selfTypeParents =
              val givenSelf = classInfo.givenSelfType
              if givenSelf.isValueType then
                givenSelf.classSymbols.filter(_ != classSym)
              else
                Nil

            (selfTypeParents ++ parentSymbols).distinct.map(SemanticdbSymbols.symbolName)
          else Nil
        val allParents =
          val visited = mutable.Set[Symbol]()
          def collect(sym: Symbol): Unit =
            visited += sym
            if sym.isClass
            then
              sym.asClass.parentSyms.foreach {
                case parent if !visited(parent) =>
                  collect(parent)
                case _ =>
              }
              val givenSelf = sym.asClass.givenSelfType
              if givenSelf.isValueType then
                givenSelf.classSymbols.foreach { selfParent =>
                  if !visited(selfParent) && selfParent != sym then
                    collect(selfParent)
                }
          collect(classSym)
          visited.toList.map(SemanticdbSymbols.symbolName)
        val dealisedSymbol =
          if sym.isAliasType then sym.info.deepDealiasAndSimplify.typeSymbol else sym
        val classOwner =
          sym.ownersIterator.drop(1).find(s => s.isClass || s.is(Flags.Module))
        val overridden = sym.denot.allOverriddenSymbols.toList
        val memberDefAnnots =
          if classSym.exists then
            classSym.info
              .membersBasedOnFlags(Flags.Method, Flags.EmptyFlags)
              .flatMap(_.allSymbols)
              .flatMap(_.denot.annotations)
          else Nil

        val pcSymbolInformation =
          PcSymbolInformation(
            symbol = SemanticdbSymbols.symbolName(sym),
            kind = getSymbolKind(sym),
            parents = parents,
            dealiasedSymbol = SemanticdbSymbols.symbolName(dealisedSymbol),
            classOwner = classOwner.map(SemanticdbSymbols.symbolName),
            overriddenSymbols = overridden.map(SemanticdbSymbols.symbolName),
            alternativeSymbols =
              alternativeSymbols.map(SemanticdbSymbols.symbolName),
            properties =
              if sym.is(Flags.Abstract) then List(PcSymbolProperty.ABSTRACT)
              else Nil,
            recursiveParents = allParents,
            annotations = sym.denot.annotations.map(_.symbol.showFullName),
            memberDefsAnnotations = memberDefAnnots.map(_.symbol.showFullName).toList
          )

        Some(pcSymbolInformation)
    end match
  end info

  private def getSymbolKind(sym: Symbol): PcSymbolKind =
    if sym.isAllOf(Flags.JavaInterface) then PcSymbolKind.INTERFACE
    else if sym.is(Flags.Trait) then PcSymbolKind.TRAIT
    else if sym.isConstructor then PcSymbolKind.CONSTRUCTOR
    else if sym.isPackageObject then PcSymbolKind.PACKAGE_OBJECT
    else if sym.isClass then PcSymbolKind.CLASS
    else if sym.is(Flags.Macro) then PcSymbolKind.MACRO
    else if sym.is(Flags.Local) then PcSymbolKind.LOCAL
    else if sym.is(Flags.Method) then PcSymbolKind.METHOD
    else if sym.is(Flags.Param) then PcSymbolKind.PARAMETER
    else if sym.is(Flags.Package) then PcSymbolKind.PACKAGE
    else if sym.is(Flags.TypeParam) then PcSymbolKind.TYPE_PARAMETER
    else if sym.isType then PcSymbolKind.TYPE
    else PcSymbolKind.UNKNOWN_KIND
end SymbolInformationProvider

object SymbolProvider:

  def compilerSymbol(symbol: String)(using Context): Option[Symbol] =
    compilerSymbols(symbol).find(sym => SemanticdbSymbols.symbolName(sym) == symbol)

  def compilerSymbols(symbol: String)(using Context): List[Symbol] =
    try toSymbols(SymbolInfo.getPartsFromSymbol(symbol))
    catch case NonFatal(e) => Nil

  private def normalizePackage(pkg: String): String =
    pkg.replace("/", ".").stripSuffix(".")

  private def toSymbols(info: SymbolInfo.SymbolParts)(using Context): List[Symbol] =
    def collectSymbols(denotation: Denotation): List[Symbol] =
      denotation match
        case MultiDenotation(denot1, denot2) =>
          collectSymbols(denot1) ++ collectSymbols(denot2)
        case denot => List(denot.symbol)

    def loop(
        owners: List[Symbol],
        parts: List[(String, Boolean)]
    ): List[Symbol] =
      parts match
        case (head, isClass) :: tl =>
          val foundSymbols =
            owners.flatMap { owner =>
              val name = head.stripBackticks
              val next =
                if isClass then owner.info.member(typeName(name))
                else owner.info.member(termName(name))
              collectSymbols(next).filter(_.exists)
            }
          if foundSymbols.nonEmpty then loop(foundSymbols, tl)
          else Nil
        case Nil => owners

    val pkgSym =
      if info.packagePart == "_empty_/" then requiredPackage(nme.EMPTY_PACKAGE)
      else requiredPackage(normalizePackage(info.packagePart))
    val found = loop(List(pkgSym), info.names)
    info.paramName match
      case Some(name) => found.flatMap(_.paramSymss.flatten.find(_.showName == name))
      case _ => found
  end toSymbols
