package dotty.tools
package dotc
package semanticdb

import dotty.tools.dotc.{semanticdb => s}
import SymbolInformation.{Kind => k}
import dotty.tools.dotc.semanticdb.Scala3.SymbolKind
import Scala3.Symbols

import core.NameKinds
import core.Symbols._
import core.Flags._
import core.Contexts.Context
import ast.tpd._
import core.NameOps._

import scala.collection.mutable

object SymbolInformationOps:
  extension (sym: Symbol)
    def toSymbolInformation(using LinkMode, Context, SemanticSymbolBuilder): s.SymbolInformation =
      // val symkinds = sym.defTree.symbolKinds
      sym.symbolInfo(Set.empty)

    def symbolInfo(symkinds: Set[SymbolKind])(using LinkMode, Context, SemanticSymbolBuilder): SymbolInformation =
      import s.SymbolOps._
      val sname = sym.symbolName
      val signature = sym.sig
      val kind = symbolKind(symkinds)
      SymbolInformation(
        symbol = sname,
        language = Language.SCALA,
        kind = kind,
        properties = sym.symbolProps(symkinds),
        displayName = Symbols.displaySymbol(sym),
        signature = signature,
        access = symbolAccess(kind),
      )

    private def symbolKind(symkinds: Set[SymbolKind])(using Context): SymbolInformation.Kind =
      if sym.isTypeParam then
        SymbolInformation.Kind.TYPE_PARAMETER
      else if sym.is(TermParam) then
        SymbolInformation.Kind.PARAMETER
      else if sym.isTerm && sym.owner.isTerm then
        SymbolInformation.Kind.LOCAL
      else if sym.isInlineMethod || sym.is(Macro) then
        SymbolInformation.Kind.MACRO
      else if sym.isConstructor then
        SymbolInformation.Kind.CONSTRUCTOR
      else if sym.isSelfSym then
        SymbolInformation.Kind.SELF_PARAMETER
      else if sym.isOneOf(Method) || symkinds.exists(_.isVarOrVal) then
        SymbolInformation.Kind.METHOD
      else if sym.isPackageObject then
        SymbolInformation.Kind.PACKAGE_OBJECT
      else if sym.is(Module) then
        SymbolInformation.Kind.OBJECT
      else if sym.is(Package) then
        SymbolInformation.Kind.PACKAGE
      else if sym.isAllOf(JavaInterface) then
        SymbolInformation.Kind.INTERFACE
      else if sym.is(Trait) then
        SymbolInformation.Kind.TRAIT
      else if sym.isClass then
        SymbolInformation.Kind.CLASS
      else if sym.isType then
        SymbolInformation.Kind.TYPE
      else if sym.is(ParamAccessor) then
        SymbolInformation.Kind.FIELD
      else
        SymbolInformation.Kind.UNKNOWN_KIND

    private def symbolProps(symkinds: Set[SymbolKind])(using Context): Int =
      if sym.is(ModuleClass) then
        return sym.sourceModule.symbolProps(symkinds)
      var props = 0
      if sym.isPrimaryConstructor then
        props |= SymbolInformation.Property.PRIMARY.value
      if sym.is(Abstract) || symkinds.contains(SymbolKind.Abstract) then
        props |= SymbolInformation.Property.ABSTRACT.value
      if sym.is(Final) then
        props |= SymbolInformation.Property.FINAL.value
      if sym.is(Sealed) then
        props |= SymbolInformation.Property.SEALED.value
      if sym.isOneOf(GivenOrImplicit) then
        props |= SymbolInformation.Property.IMPLICIT.value
      if sym.is(Lazy, butNot=Module) then
        props |= SymbolInformation.Property.LAZY.value
      if sym.isAllOf(Case | Module) || sym.is(CaseClass) || sym.isAllOf(EnumCase) then
        props |= SymbolInformation.Property.CASE.value
      if sym.is(Covariant) then
        props |= SymbolInformation.Property.COVARIANT.value
      if sym.is(Contravariant) then
        props |= SymbolInformation.Property.CONTRAVARIANT.value
      if sym.isAllOf(DefaultMethod | JavaDefined) || sym.is(Accessor) && sym.name.is(NameKinds.DefaultGetterName) then
        props |= SymbolInformation.Property.DEFAULT.value
      if symkinds.exists(_.isVal) then
        props |= SymbolInformation.Property.VAL.value
      if symkinds.exists(_.isVar) then
        props |= SymbolInformation.Property.VAR.value
      if sym.is(JavaStatic) then
        props |= SymbolInformation.Property.STATIC.value
      if sym.is(Enum) then
        props |= SymbolInformation.Property.ENUM.value
      if sym.is(Given) then
        props |= SymbolInformation.Property.GIVEN.value
      if sym.is(Inline) then
        props |= SymbolInformation.Property.INLINE.value
      if sym.is(Open) then
        props |= SymbolInformation.Property.OPEN.value
      if sym.is(Open) then
        props |= SymbolInformation.Property.OPEN.value
      if sym.is(Transparent) then
        props |= SymbolInformation.Property.TRANSPARENT.value
      if sym.is(Infix) then
        props |= SymbolInformation.Property.INFIX.value
      if sym.is(Opaque) then
        props |= SymbolInformation.Property.OPAQUE.value
      props

    private def symbolAccess(kind: SymbolInformation.Kind)(using Context, SemanticSymbolBuilder): Access =
      kind match
        case k.LOCAL | k.PARAMETER | k.SELF_PARAMETER | k.TYPE_PARAMETER | k.PACKAGE | k.PACKAGE_OBJECT =>
          Access.Empty
        case _ =>
          if (sym.privateWithin == NoSymbol)
            if (sym.isAllOf(PrivateLocal)) PrivateThisAccess()
            else if (sym.is(Private)) PrivateAccess()
            else if (sym.isAllOf(ProtectedLocal)) ProtectedThisAccess()
            else if (sym.is(Protected)) ProtectedAccess()
            else PublicAccess()
          else
            val ssym = sym.privateWithin.symbolName
            if (sym.is(Protected)) ProtectedWithinAccess(ssym)
            else PrivateWithinAccess(ssym)


  /**Necessary because not all of the eventual flags are propagated from the Tree to the symbol yet.
   */
  extension (tree: Tree)
    def symbolKinds(using Context): Set[SymbolKind] =
      if tree.symbol.isSelfSym then
        Set.empty
      else
        val symkinds = mutable.HashSet.empty[SymbolKind]
        tree match
          case tree: ValDef =>
            if !tree.symbol.is(Param) then
              symkinds += (if tree.mods is Mutable then SymbolKind.Var else SymbolKind.Val)
            if tree.rhs.isEmpty && !tree.symbol.isOneOf(TermParam | CaseAccessor | ParamAccessor) then
              symkinds += SymbolKind.Abstract
          case tree: DefDef =>
            if tree.isSetterDef then
              symkinds += SymbolKind.Setter
            else if tree.rhs.isEmpty then
              symkinds += SymbolKind.Abstract
          case tree: Bind =>
            symkinds += SymbolKind.Val
          case _ =>
        symkinds.toSet

  extension (tree: DefDef)
    private def isSetterDef(using Context): Boolean =
      tree.name.isSetterName && tree.mods.is(Accessor) && tree.termParamss.isSingleArg

  extension (list: List[List[ValDef]])
    private  inline def isSingleArg = list match
      case (_::Nil)::Nil => true
      case _             => false
