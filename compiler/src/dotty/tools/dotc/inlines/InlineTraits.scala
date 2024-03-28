package dotty.tools.dotc.inlines

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.core.Constants.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.NameOps.*
import dotty.tools.dotc.core.Names.TypeName
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Scopes.newScope
import dotty.tools.dotc.report
import dotty.tools.dotc.util.SrcPos
import dotty.tools.dotc.util.Spans.Span

object InlineTraits:
  import tpd.*

  def adaptNoInit(cls: ClassSymbol, parents1: List[Tree])(using Context): Unit =
    if parents1.exists(parent => parent.symbol.isInlineTrait && !parent.symbol.is(NoInits)) then
      cls.resetFlag(NoInits)

  def needsTraitInlining(cls: ClassSymbol)(using Context): Boolean =
    !cls.isInlineTrait
    && cls.info.parents.exists(parent => parent.typeSymbol.isInlineTrait)

  /** Generate all inlined definitions for all inline parents of `cls`.
   *  New definition symbol are not entered in the class `cls`.
   */
  def inlinedMemberSymbols(cls: ClassSymbol)(using Context): List[Symbol] =
    assert(!cls.isInlineTrait, cls)
    for
      denot <- cls.typeRef.allMembers.toList
      sym = denot.symbol
      if isInlinableMember(sym)
    yield
      val traitTargs = parentTargs(cls, sym)
      if sym.isClass then inlinedSymbolClassDef(cls, sym.asClass, traitTargs)
      else inlinedSymbolValOrDef(cls, sym, traitTargs)
  end inlinedMemberSymbols

  private def isInlinableMember(sym: Symbol)(using Context): Boolean =
    (sym.isTerm || sym.isClass)
    && !sym.isConstructor && !sym.is(ParamAccessor)
    && sym.owner.isInlineTrait

  private def parentTargs(cls: ClassSymbol, inlinableDecl: Symbol)(using Context): List[Type] =
    val baseClass = inlinableDecl.owner.asClass
    mixinParentTypeOf(cls, baseClass).baseType(baseClass) match
      case AppliedType(_, targs) => targs
      case _ => Nil

  private def inlinedSymbolValOrDef(cls: ClassSymbol, inlinableDecl: Symbol, traitTargs: List[Type])(using Context): Symbol =
    val flags = inlinableDecl.flags | Override | Synthetic
    val info = inlinableDecl.info
      .substThis(inlinableDecl.owner.asClass, ThisType.raw(cls.typeRef))
      .subst(inlinableDecl.owner.typeParams, traitTargs)
    val privateWithin = inlinableDecl.privateWithin // TODO what should `privateWithin` be?
    newSymbol(cls, inlinableDecl.name, flags, info, privateWithin, cls.span)

  private def inlinedSymbolClassDef(cls: ClassSymbol, inlinableDecl: ClassSymbol, traitTargs: List[Type])(using Context): ClassSymbol =
    def infoFn(cls1: ClassSymbol) =
      inlinableDecl.info.asInstanceOf[ClassInfo].derivedClassInfo(
        prefix = cls.typeRef,
        declaredParents = defn.ObjectType :: cls.thisType.select(inlinableDecl) :: Nil,
        decls = newScope,
        // selfInfo = ,
      )

    val newCls = newClassSymbol(
      owner = cls,
      name = inlinableDecl.name.toTypeName,
      flags = inlinableDecl.flags | Synthetic,
      infoFn = infoFn,
      privateWithin = NoSymbol,
      coord = cls.coord
    )

    newConstructor(
      newCls,
      flags = EmptyFlags,
      paramNames = Nil,
      paramTypes = Nil,
      privateWithin = NoSymbol,
      coord = newCls.coord
    ).entered

    for
      decl <- inlinableDecl.info.decls.toList
      if decl.isTerm && !decl.isConstructor && !decl.is(ParamAccessor)
    do
      inlinedSymbolValOrDef(newCls, decl, traitTargs).entered

    newCls
  end inlinedSymbolClassDef

  def inlinedDefs(cls: ClassSymbol)(using Context): List[Tree] =
    atPhase(ctx.phase.next) { cls.info.decls.toList }
      .filter(sym => sym.is(Synthetic) && sym.nextOverriddenSymbol.maybeOwner.isInlineTrait)
      .map { sym =>
        if sym.isClass then inlinedClassDefs(cls, sym.asClass)
        else inlinedValOrDefDefs(cls, sym)
      }

  private def inlinedValOrDefDefs(cls: ClassSymbol, inlinedDecl: Symbol)(using Context): Tree =
    val inlinableDecl = inlinedDecl.allOverriddenSymbols.find { sym =>
      !sym.is(Deferred) && sym.owner.isInlineTrait
    }.getOrElse(inlinedDecl.nextOverriddenSymbol)
    val parent = mixinParentTypeOf(inlinedDecl.owner.asClass, inlinableDecl.owner.asClass).typeSymbol.name.asTypeName
    valOrDefDefInlineOverride(cls, inlinedDecl, parent, inlinableDecl)

  private def inlinedClassDefs(cls: ClassSymbol, inlinedDecl: ClassSymbol)(using Context): Tree =
    val parent = inlinedDecl.info.parents.last.typeSymbol
    val members = parent.info.decls.toList.filterNot(_.is(ParamAccessor)).zip(inlinedDecl.info.decls.toList).collect {
      case (overridden, decl) if decl.isTerm && !decl.isConstructor && !decl.is(ParamAccessor) =>
        assert(overridden.name == decl.name, (overridden, decl)) // TODO find better wy to recover `overridden` from `decl`
        val parent = mixinParentTypeOf(decl.owner.asClass, overridden.owner.asClass).typeSymbol.name.asTypeName
        valOrDefDefInlineOverride(cls, decl, parent, overridden)
    }
    ClassDef(
      inlinedDecl.asClass,
      DefDef(inlinedDecl.primaryConstructor.asTerm),
      body = members,
      superArgs = List.empty[Tree]
    ).withSpan(inlinedDecl.span)

  private def valOrDefDefInlineOverride(cls: ClassSymbol, decl: Symbol, parent: TypeName, overridden: Symbol)(using Context): Tree =
    def rhs(argss: List[List[Tree]])(using Context) =
      if decl.is(Deferred) then EmptyTree
      else if decl.is(Mutable) && decl.name.isSetterName then Literal(Constant(()))
      else
        ctx.compilationUnit.needsInlining = true
        Super(This(ctx.owner.owner.asClass), parent).select(overridden).appliedToArgss(argss)

    if decl.is(Method) then DefDef(decl.asTerm, rhs(_)(using ctx.withOwner(decl))).withSpan(cls.span)
    else ValDef(decl.asTerm, rhs(Nil)(using ctx.withOwner(decl))).withSpan(cls.span)

  private def mixinParentTypeOf(cls: ClassSymbol, baseClass: ClassSymbol)(using Context): Type =
    cls.info.parents.findLast(parent => parent.typeSymbol.derivesFrom(baseClass)).get

  /** Register inline members RHS in `@bodyAnnotation`s */
  def registerInlineTraitInfo(stats: List[Tree])(using Context): Unit =
    for stat <- stats do
      stat match
        case stat: ValOrDefDef if !stat.symbol.is(Inline) && !stat.symbol.is(Deferred) =>
          // TODO? val rhsToInline = PrepareInlineable.wrapRHS(stat, stat.tpt, stat.rhs)
          PrepareInlineable.registerInlineInfo(stat.symbol, stat.rhs/*TODO? rhsToInline*/)
        case TypeDef(_, rhs: Template) =>
          registerInlineTraitInfo(rhs.body)
        case _ =>

  /** Checks if members are supported in inline traits */
  def checkValidInlineTraitMember(stats: List[Tree])(using Context): Unit =
    for stat <- stats do
      val sym = stat.symbol
      stat match
        case stat: ValOrDefDef =>
          if sym.is(Module) then report.error(em"Implementation restriction: object cannot be defined in inline traits", stat.srcPos)
          else if sym.is(Private) then report.error(em"Implementation restriction: private ${sym.kindString} cannot be defined in inline traits", stat.srcPos)
          else () // Ok
        case stat: TypeDef =>
          if sym.isClass && !sym.is(Trait) then report.error(em"Implementation restriction: ${sym.kindString} cannot be defined in inline traits", stat.srcPos)
          else () // OK
        case _: Import =>
          report.error(em"Implementation restriction: import cannot be defined in inline traits", stat.srcPos)
        case _: Export =>
          report.error(em"Implementation restriction: export cannot be defined in inline traits", stat.srcPos)
        case _ =>
          report.error(em"Implementation restriction: statements cannot be added to inline traits", stat.srcPos)
