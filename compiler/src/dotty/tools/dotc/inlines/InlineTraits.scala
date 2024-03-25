package dotty.tools.dotc.inlines

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.core.Constants.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.NameOps.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
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
    val parents = cls.info.parents

    def parentTargs(inlinableDecl: Symbol): List[Type] =
      val baseClass = inlinableDecl.owner.asClass
      mixinParentTypeOf(cls, baseClass).baseType(baseClass) match
        case AppliedType(_, targs) => targs
        case _ => Nil

    def inlinedSymbol(inlinableDecl: Symbol, traitTargs: List[Type]): Symbol =
      val flags = inlinableDecl.flags | Override | Synthetic
      val info = inlinableDecl.info
        .substThis(inlinableDecl.owner.asClass, ThisType.raw(cls.typeRef))
        .subst(inlinableDecl.owner.typeParams, traitTargs)
      val privateWithin = inlinableDecl.privateWithin // TODO what should `privateWithin` be?
      newSymbol(cls, inlinableDecl.name, flags, info, privateWithin, cls.span)

    def needsInlinedDecl(sym: Symbol): Boolean =
      sym.isTerm && !sym.isConstructor && !sym.is(ParamAccessor)
      && sym.owner.isInlineTrait

    for
      denot <- cls.typeRef.allMembers.toList
      inlinableDecl = denot.symbol
      if needsInlinedDecl(inlinableDecl)
    yield
      inlinedSymbol(inlinableDecl, parentTargs(inlinableDecl))

  end inlinedMemberSymbols


  def inlinedDefs(cls: ClassSymbol)(using Context): List[Tree] =
    def makeValOrDef(inlinedDecl: Symbol): Tree =
      def makeSuperSelect(using Context) =
        ctx.compilationUnit.needsInlining = true
        val inlinableDecl = inlinedDecl.allOverriddenSymbols.find { sym =>
          !sym.is(Deferred) && sym.owner.isInlineTrait
        }.getOrElse(inlinedDecl.nextOverriddenSymbol)
        val parent = mixinParentTypeOf(cls, inlinableDecl.owner.asClass)
        Super(This(cls), parent.typeSymbol.name.asTypeName).select(inlinableDecl)

      def rhs(using Context)(argss: List[List[Tree]]) =
        if inlinedDecl.is(Deferred) then EmptyTree
        else if inlinedDecl.is(Mutable) && inlinedDecl.name.isSetterName then Literal(Constant(()))
        else makeSuperSelect.appliedToArgss(argss)

      if inlinedDecl.is(Method) then DefDef(inlinedDecl.asTerm, rhs(using ctx.withOwner(inlinedDecl))).withSpan(cls.span)
      else ValDef(inlinedDecl.asTerm, rhs(using ctx.withOwner(inlinedDecl))(Nil)).withSpan(cls.span)

    atPhase(ctx.phase.next) { cls.info.decls.toList }
      .filter(sym => sym.is(Synthetic) && sym.nextOverriddenSymbol.maybeOwner.isInlineTrait)
      .map(makeValOrDef)

  end inlinedDefs

  private def mixinParentTypeOf(cls: ClassSymbol, baseClass: ClassSymbol)(using Context): Type =
    cls.info.parents.findLast(parent => parent.typeSymbol.derivesFrom(baseClass)).get

  /** Register inline members RHS in `@bodyAnnotation`s */
  def registerInlineTraitInfo(cls: ClassSymbol, stats: List[Tree])(using Context): Unit =
    for stat <- stats do
      stat match
        case stat: ValOrDefDef if !stat.symbol.is(Inline) && !stat.symbol.is(Deferred) =>
          // TODO? val rhsToInline = PrepareInlineable.wrapRHS(stat, stat.tpt, stat.rhs)
          PrepareInlineable.registerInlineInfo(stat.symbol, stat.rhs/*TODO? rhsToInline*/)
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
          if sym.isClass then report.error(em"Implementation restriction: ${sym.kindString} cannot be defined in inline traits", stat.srcPos)
          else () // OK
        case _: Import =>
          report.error(em"Implementation restriction: import cannot be defined in inline traits", stat.srcPos)
        case _: Export =>
          report.error(em"Implementation restriction: export cannot be defined in inline traits", stat.srcPos)
        case _ =>
          report.error(em"Implementation restriction: statements cannot be added to inline traits", stat.srcPos)
