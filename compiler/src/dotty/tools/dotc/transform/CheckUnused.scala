package dotty.tools.dotc.transform

import scala.annotation.tailrec

import dotty.tools.uncheckedNN
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.ast.untpd, untpd.ImportSelector
import dotty.tools.dotc.config.ScalaSettings
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.{em, i, toMessage}
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Types.{AnnotatedType, ClassInfo, ConstantType, NamedType, NoType, TermRef, Type, TypeProxy, TypeTraverser}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.{Name, TermName, termName}
import dotty.tools.dotc.core.NameOps.isReplWrapperName
import dotty.tools.dotc.core.NameKinds.WildcardParamName
import dotty.tools.dotc.core.Symbols.{NoSymbol, Symbol, defn, isDeprecated}
import dotty.tools.dotc.report
import dotty.tools.dotc.reporting.{Message, UnusedSymbol as UnusedSymbolMessage}
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.typer.ImportInfo
import dotty.tools.dotc.util.{Property, SrcPos}
import dotty.tools.dotc.util.Spans.Span
import scala.util.chaining.given

/** A compiler phase that checks for unused imports or definitions.
 *
 *  Every construct that introduces a name must have at least one corresponding reference.
 *  The analysis is restricted to definitions of limited scope, i.e., private and local definitions.
 */
class CheckUnused private (phaseMode: CheckUnused.PhaseMode, suffix: String, _key: Property.Key[CheckUnused.UnusedData]) extends MiniPhase:
  import CheckUnused.*
  import UnusedData.*

  private inline def ud(using ud: UnusedData): UnusedData = ud

  private inline def preparing[U](inline op: UnusedData ?=> U)(using ctx: Context): ctx.type =
    ctx.property(_key) match
      case Some(ud) => op(using ud)
      case None     =>
    ctx

  override def phaseName: String = CheckUnused.phaseNamePrefix + suffix

  override def description: String = CheckUnused.description

  override def isEnabled(using Context): Boolean = ctx.settings.WunusedHas.any

  override def isRunnable(using Context): Boolean = super.isRunnable && ctx.settings.WunusedHas.any && !ctx.isJava

  // Inherit data from previous phase.
  override def prepareForUnit(tree: Tree)(using Context): Context =
    val data = UnusedData()
    for oldData <- tree.getAttachment(_key) do
      data.unusedAggregate = oldData.unusedAggregate
    ctx.fresh.setProperty(_key, data).tap(_ => tree.putAttachment(_key, data))

  // Report if we are last phase
  override def transformUnit(tree: Tree)(using Context): tree.type =
    preparing:
      ud.finishAggregation()
      if phaseMode == PhaseMode.Report then
        ud.unusedAggregate.foreach(reportUnused)
    tree

  override def transformIdent(tree: Ident)(using Context): tree.type =
    preparing:
      if tree.symbol.exists then
        def loopOnNormalizedPrefixes(prefix: Type, depth: Int): Unit =
          // limit to 10 as failsafe for the odd case where there is an infinite cycle
          if depth < 10 && prefix.exists then
            ud.registerUsed(prefix.classSymbol, name = None, prefix)
            loopOnNormalizedPrefixes(prefix.normalizedPrefix, depth + 1)
        val prefix = tree.typeOpt.normalizedPrefix
        loopOnNormalizedPrefixes(prefix, depth = 0)
        ud.registerUsed(tree.symbol, Some(tree.name), tree.typeOpt.importPrefix.skipPackageObject)
      else if tree.hasType then
        ud.registerUsed(tree.tpe.classSymbol, Some(tree.name), tree.tpe.importPrefix.skipPackageObject)
    tree

  override def transformSelect(tree: Select)(using Context): tree.type =
    preparing:
      val name = tree.removeAttachment(OriginalName)
      ud.registerUsed(tree.symbol, name, tree.qualifier.tpe, includeForImport = tree.qualifier.span.isSynthetic)
    tree

  override def transformApply(tree: Apply)(using Context): Tree =
    //println(s"APPLY ${tree.show}")
    tree

  override def transformTyped(tree: Typed)(using Context): Tree =
    tree match
    case Typed(expr, tpt) =>
    tree

  override def transformAssign(tree: Assign)(using Context): tree.type =
    preparing:
      val sym = tree.lhs.symbol
      if sym.exists then
        ud.registerSetVar(sym)
    tree

  override def prepareForBlock(tree: Block)(using Context): Context =
    pushScope(tree)

  override def transformBlock(tree: Block)(using Context): tree.type =
    popScope(tree)
    tree

  override def transformInlined(tree: Inlined)(using Context): tree.type =
    transformAllDeep(tree.call)
    tree

  override def prepareForTypeTree(tree: TypeTree)(using Context): Context = ctx

  override def transformTypeTree(tree: TypeTree)(using Context): tree.type =
    //println(s"TYPETREE ${tree.getClass} ${tree.show} or $tree")
    tree.tpe match
    case AnnotatedType(_, annot) => transformAllDeep(annot.tree)
    case _ =>
    tree

  override def transformBind(tree: Bind)(using Context): tree.type =
    preparing:
      traverseAnnotations(tree.symbol)
      ud.registerPatVar(tree)
    tree

  override def prepareForValDef(tree: ValDef)(using Context): Context =
    preparing:
      ud.addIgnoredUsage(tree.symbol)

  override def transformValDef(tree: ValDef)(using Context): tree.type =
    //println(s"VAL ${tree.name} ${tree.show} tpt ${tree.tpt.show} is a ${tree.tpt.getClass}")
    preparing:
      traverseAnnotations(tree.symbol)
      if !tree.symbol.is(Module) then // do not register the ValDef generated for `object`
        ud.registerDef(tree)
      if tree.name.startsWith("derived$") && tree.hasType then
        def core(t: Tree): (Symbol, Option[Name], Type) = t match
          case Ident(name)  => (t.tpe.typeSymbol, Some(name), t.tpe.underlyingPrefix)
          case Select(t, _) => core(t)
          case _            => (NoSymbol, None, NoType)
        tree.getAttachment(OriginalTypeClass) match
        case Some(orig) =>
          val (typsym, name, prefix) = core(orig)
          ud.registerUsed(typsym, name, prefix.skipPackageObject)
        case _ =>
      ud.removeIgnoredUsage(tree.symbol)
    tree

  override def prepareForDefDef(tree: DefDef)(using Context): Context =
    preparing:
      ud.registerTrivial(tree)
      if !tree.symbol.is(Private) then
        tree.termParamss.flatten.foreach(p => ud.addIgnoredParam(p.symbol))
      ud.addIgnoredUsage(tree.symbol)

  override def transformDefDef(tree: DefDef)(using Context): tree.type =
    preparing:
      traverseAnnotations(tree.symbol)
      ud.registerDef(tree)
      ud.removeIgnoredUsage(tree.symbol)
    tree

  override def prepareForTypeDef(tree: TypeDef)(using Context): Context =
    preparing:
      ud.addIgnoredUsage(tree.symbol)

  override def transformTypeDef(tree: TypeDef)(using Context): tree.type =
    preparing:
      traverseAnnotations(tree.symbol)
      if !tree.symbol.is(Param) then // type parameter to do?
        ud.registerDef(tree)
      ud.removeIgnoredUsage(tree.symbol)
    tree

  override def prepareForTemplate(tree: Template)(using Context): Context =
    pushScope(tree)

  override def transformTemplate(tree: Template)(using Context): Tree =
    popScope(tree)
    tree

  override def prepareForPackageDef(tree: PackageDef)(using Context): Context =
    pushScope(tree)

  override def transformPackageDef(tree: PackageDef)(using Context): Tree =
    popScope(tree)
    tree

  override def prepareForStats(trees: List[Tree])(using Context): Context = ctx

  override def transformStats(trees: List[Tree])(using Context): List[Tree] =
    super.transformStats(trees)
    trees

  override def transformOther(tree: Tree)(using Context): tree.type =
    preparing:
      tree match
      case imp: Import =>
        ud.registerImport(imp)
        transformAllDeep(imp.expr)
        for selector <- imp.selectors do
          if selector.isGiven then
            selector.bound match
            case untpd.TypedSplice(bound) => transformAllDeep(bound)
            case _ =>
      case _: InferredTypeTree =>
        //println(s"INF ${tree.getClass} ${tree.show}")
      case AppliedTypeTree(tpt, args) =>
        transformAllDeep(tpt)
        args.foreach(transformAllDeep)
      case RefinedTypeTree(tpt, refinements) =>
        transformAllDeep(tpt)
        refinements.foreach(transformAllDeep)
      case LambdaTypeTree(tparams, body) =>
        tparams.foreach(transformAllDeep)
        transformAllDeep(body)
      case SingletonTypeTree(ref) =>
        transformAllDeep(ref)
      case TypeBoundsTree(lo, hi, alias) =>
        transformAllDeep(lo)
        transformAllDeep(hi)
        transformAllDeep(alias)
      case tree: NamedArg => transformAllDeep(tree.arg)
      case _ if tree.isType =>
        //println(s"OTHER TYPE ${tree.getClass} ${tree.show}")
      case _ =>
        //println(s"OTHER ${tree.getClass} ${tree.show}")
    tree

  private def pushScope(tree: Block | Template | PackageDef)(using Context): Context =
    preparing:
      ud.pushScope(UnusedData.ScopeType.fromTree(tree))

  private def popScope(tree: Block | Template | PackageDef)(using Context): Context =
    preparing:
      ud.popScope(UnusedData.ScopeType.fromTree(tree))

  private def traverseAnnotations(sym: Symbol)(using Context): Unit =
    for annot <- sym.denot.annotations do
      transformAllDeep(annot.tree)

  /** Do the actual reporting given the result of the anaylsis */
  private def reportUnused(res: UnusedData.UnusedResult)(using Context): Unit =
    def messageFor(w: WarnTypes): Message =
      import WarnTypes.*, UnusedSymbolMessage.*
      w match
      case Imports        => imports
      case LocalDefs      => localDefs
      case ExplicitParams => explicitParams
      case ImplicitParams => implicitParams
      case PrivateMembers => privateMembers
      case PatVars        => patVars
      case UnsetLocals    => "unset local variable, consider using an immutable val instead".toMessage
      case UnsetPrivates  => "unset private variable, consider using an immutable val instead".toMessage
    res.warnings.toArray.sortInPlaceBy(_.pos.span.point).foreach:
      case UnusedSymbol(pos, _, warnType) =>
        report.warning(messageFor(warnType), pos)

end CheckUnused

object CheckUnused:
  val phaseNamePrefix: String = "checkUnused"
  val description: String = "check for unused elements"

  enum PhaseMode:
    case Aggregate
    case Report

  private enum WarnTypes:
    case Imports
    case LocalDefs
    case ExplicitParams
    case ImplicitParams
    case PrivateMembers
    case PatVars
    case UnsetLocals
    case UnsetPrivates

  /** The key used to retrieve the "unused entity" analysis metadata from the compilation `Context` */
  private val _key = Property.StickyKey[UnusedData]

  /** Attachment holding the name of an Ident as written by the user. */
  val OriginalName = Property.StickyKey[Name]

  /** Attachment holding the name of a type class as written by the user. */
  val OriginalTypeClass = Property.StickyKey[Tree]

  class PostTyper extends CheckUnused(PhaseMode.Aggregate, "PostTyper", _key)

  class PostInlining extends CheckUnused(PhaseMode.Report, "PostInlining", _key)

  /** Track usages at a Context.
   *
   *  For an ImportContext, which selectors have been used for lookups?
   *
   *  For other contexts, which symbols defined here have been referenced?
   */
  private class UnusedData:
    import collection.mutable as mut, mut.Stack, mut.ListBuffer
    import UnusedData.*

    /** The current scope during the tree traversal */
    val currScopeType: Stack[ScopeType] = Stack(ScopeType.Other)
    inline def peekScopeType = currScopeType.top

    var unusedAggregate: Option[UnusedResult] = None

    /* IMPORTS */
    private val impInScope = Stack(ListBuffer.empty[ImportSelectorData])
    private val usedInScope = Stack(mut.Map.empty[Symbol, ListBuffer[Usage]])
    private val usedInPosition = mut.Map.empty[Name, mut.Set[Symbol]]
    /* unused import collected during traversal */
    private val unusedImport = ListBuffer.empty[ImportSelectorData]

    /* LOCAL DEF OR VAL / Private Def or Val / Pattern variables */
    private val localDefInScope = ListBuffer.empty[MemberDef]
    private val privateDefInScope = ListBuffer.empty[MemberDef]
    private val explicitParamInScope = ListBuffer.empty[MemberDef]
    private val implicitParamInScope = ListBuffer.empty[MemberDef]
    private val patVarsInScope = ListBuffer.empty[Bind]

    /** All variables sets*/
    private val setVars = mut.Set.empty[Symbol]

    /** All used symbols */
    private val usedDef = mut.Set.empty[Symbol]
    /** Do not register as used */
    private val doNotRegister = mut.Set.empty[Symbol]

    /** Trivial definitions, avoid registering params */
    private val trivialDefs = mut.Set.empty[Symbol]

    private val paramsToSkip = mut.Set.empty[Symbol]

    def finishAggregation(using Context)(): Unit =
      unusedAggregate = unusedAggregate match
        case None =>
          Some(getUnused)
        case Some(prevUnused) =>
          Some(UnusedResult(getUnused.warnings.intersect(prevUnused.warnings)))

    def registerSelectors(selectors: List[ImportSelectorData]): this.type =
      impInScope.top.prependAll(selectors)
      this

    /** Register a found (used) symbol along with its name.
     *
     *  The optional name will be used to target the right import
     *  as the same element can be imported with different renaming.
     */
    def registerUsed(sym: Symbol, name: Option[Name], prefix: Type = NoType, includeForImport: Boolean = true)(using Context): Unit =
      if sym.exists && !isConstructorOfSynth(sym) && !doNotRegister(sym) then
        if sym.isConstructor then
          // constructors are "implicitly" imported with the class
          registerUsed(sym.owner, name = None, prefix, includeForImport = includeForImport)
        else
          // If the symbol is accessible in this scope without an import, do not register it for unused import analysis
          val includeForImport1 =
            includeForImport && (name.exists(_.toTermName != sym.name.toTermName) || !sym.isAccessibleAsIdent)

          def addIfExists(sym: Symbol): Unit =
            if sym.exists then
              usedDef += sym
              if includeForImport1 then
                addUsage(Usage(sym, name, prefix))
          addIfExists(sym)
          addIfExists(sym.companionModule)
          addIfExists(sym.companionClass)
          if sym.sourcePos.exists then
            for n <- name do
              usedInPosition.getOrElseUpdate(n, mut.Set.empty) += sym

    def addUsage(usage: Usage)(using Context): Unit =
      val usages = usedInScope.top.getOrElseUpdate(usage.symbol, ListBuffer.empty)
      if !usages.exists(x => x.name == usage.name && x.prefix =:= usage.prefix)
      then usages += usage

    /** Register a symbol that should be ignored */
    def addIgnoredUsage(sym: Symbol)(using Context): Unit =
      doNotRegister ++= sym.everySymbol

    /** Remove a symbol that shouldn't be ignored anymore */
    def removeIgnoredUsage(sym: Symbol)(using Context): Unit =
      doNotRegister --= sym.everySymbol

    def addIgnoredParam(sym: Symbol)(using Context): Unit =
      paramsToSkip += sym

    /** Register an import */
    def registerImport(imp: Import)(using Context): Unit =
      if
        !languageImport(imp.expr).nonEmpty
          && !imp.isGeneratedByEnum
          && !isTransparentAndInline(imp)
          && peekScopeType != ScopeType.ReplWrapper // #18383 Do not report top-level import's in the repl as unused
      then
        val qualTpe = imp.expr.tpe

        // Put wildcard imports at the end, because they have lower priority within one Import
        val reorderedSelectors =
          val (wildcardSels, nonWildcardSels) = imp.selectors.partition(_.isWildcard)
          nonWildcardSels ::: wildcardSels

        val newDataInScope =
          for sel <- reorderedSelectors yield
            val data = new ImportSelectorData(qualTpe, sel)
            if shouldSelectorBeReported(imp, sel) || sel.isImportExclusion || isImportIgnored(imp, sel) then
              data.markUsed() // Immediately mark the selector as used
            data
        registerSelectors(newDataInScope)
    end registerImport

    /** Register (or not) some `val` or `def` according to the context, scope and flags */
    def registerDef(memDef: MemberDef)(using Context): Unit =
      if memDef.isValidMemberDef && !isDefIgnored(memDef) then
        if memDef.isValidParam then
          if memDef.symbol.isOneOf(GivenOrImplicit) then
            if !paramsToSkip.contains(memDef.symbol) then
              implicitParamInScope += memDef
          else if !paramsToSkip.contains(memDef.symbol) then
            explicitParamInScope += memDef
        else if peekScopeType == ScopeType.Local then
          localDefInScope += memDef
        else if memDef.shouldReportPrivateDef then
          privateDefInScope += memDef

    /** Register pattern variable */
    def registerPatVar(patvar: Bind)(using Context): Unit =
      if !patvar.symbol.hasUnusedAnnot then
        patVarsInScope += patvar

    /** enter a new scope */
    def pushScope(newScopeType: ScopeType): Unit =
      currScopeType.push(newScopeType)
      impInScope.push(ListBuffer.empty)
      usedInScope.push(mut.Map.empty)

    def registerSetVar(sym: Symbol): Unit =
      setVars += sym

    /** Leave current scope and mark any used imports; collect unused imports.
     */
    def popScope(scopeType: ScopeType)(using Context): Unit =
      assert(currScopeType.pop() == scopeType)
      val selDatas = impInScope.pop()

      for usedInfos <- usedInScope.pop().valuesIterator; usedInfo <- usedInfos do
        import usedInfo.*
        selDatas.find(symbol.isInImport(_, name, prefix)) match
          case Some(sel) =>
            sel.markUsed()
          case None =>
            // Propagate the symbol one level up
            if usedInScope.nonEmpty then
              addUsage(usedInfo)
      end for // each in usedInfos

      for selData <- selDatas do
        if !selData.isUsed then
          unusedImport += selData
    end popScope

    /** Leave the scope and return a result set of warnings.
     */
    def getUnused(using Context): UnusedResult =
      popScope(ScopeType.Other) // sentinel

      def isUsedInPosition(name: Name, span: Span): Boolean =
        usedInPosition.get(name) match
          case Some(syms) => syms.exists(sym => span.contains(sym.span))
          case None       => false

      val warnings = Set.newBuilder[UnusedSymbol]

      if ctx.settings.WunusedHas.imports || ctx.settings.WunusedHas.strictNoImplicitWarn then
        warnings.addAll(unusedImport.iterator.map(d => UnusedSymbol(d.selector.srcPos, d.selector.name, WarnTypes.Imports)))

      // Partition to extract unset local variables from usedLocalDefs
      if ctx.settings.WunusedHas.locals then
        for d <- localDefInScope do
          if d.symbol.usedDefContains then
            if isUnsetVarDef(d) then
              warnings.addOne(UnusedSymbol(d.namePos, d.name, WarnTypes.UnsetLocals))
          else
            if !isUsedInPosition(d.symbol.name, d.span) && !containsSyntheticSuffix(d.symbol) then
              warnings.addOne(UnusedSymbol(d.namePos, d.name, WarnTypes.LocalDefs))

      if ctx.settings.WunusedHas.explicits then
        for d <- explicitParamInScope do
          if !d.symbol.usedDefContains && !isUsedInPosition(d.symbol.name, d.span) && !containsSyntheticSuffix(d.symbol) then
            warnings.addOne(UnusedSymbol(d.namePos, d.name, WarnTypes.ExplicitParams))

      if ctx.settings.WunusedHas.implicits then
        for d <- implicitParamInScope do
          if !d.symbol.usedDefContains && !containsSyntheticSuffix(d.symbol) then
            warnings.addOne(UnusedSymbol(d.namePos, d.name, WarnTypes.ImplicitParams))

      // Partition to extract unset private variables from usedPrivates
      if ctx.settings.WunusedHas.privates then
        for d <- privateDefInScope do
          if d.symbol.usedDefContains then
            if isUnsetVarDef(d) then
              warnings.addOne(UnusedSymbol(d.namePos, d.name, WarnTypes.UnsetPrivates))
          else
            if !containsSyntheticSuffix(d.symbol) then
              warnings.addOne(UnusedSymbol(d.namePos, d.name, WarnTypes.PrivateMembers))

      if ctx.settings.WunusedHas.patvars then
        for d <- patVarsInScope do
          if !d.symbol.usedDefContains && !isUsedInPosition(d.symbol.name, d.span) && !containsSyntheticSuffix(d.symbol) then
            warnings.addOne(UnusedSymbol(d.namePos, d.name, WarnTypes.PatVars))

      UnusedResult(warnings.result)
    end getUnused

    /** Checks if import selects a def that is transparent and inline.
     */
    private def isTransparentAndInline(imp: Import)(using Context): Boolean =
      imp.selectors.exists { sel =>
        val qual = imp.expr
        val importedMembers = qual.tpe.member(sel.name).alternatives.map(_.symbol)
        importedMembers.exists(_.isAllOf(Transparent | Inline))
      }

    /** Heuristic to detect synthetic suffixes in names of symbols.
     */
    private def containsSyntheticSuffix(symbol: Symbol)(using Context): Boolean =
      symbol.name.mangledString.contains("$")

    /**
     * Is the constructor of synthetic package object
     * Should be ignored as it is always imported/used in package
     * Trigger false negative on used import
     *
     * Without this check example:
     *
     * --- WITH PACKAGE : WRONG ---
     * {{{
     * package a:
     *   val x: Int = 0
     * package b:
     *   import a.* // no warning
     * }}}
     * --- WITH OBJECT : OK ---
     * {{{
     * object a:
     *   val x: Int = 0
     * object b:
     *   import a.* // unused warning
     * }}}
     */
    private def isConstructorOfSynth(sym: Symbol)(using Context): Boolean =
      sym.exists && sym.isConstructor && sym.owner.isPackageObject && sym.owner.is(Synthetic)

    /** This is used to avoid reporting the parameters of the synthetic main method generated by `@main`.
     */
    private def isSyntheticMainParam(sym: Symbol)(using Context): Boolean =
      sym.exists && ctx.platform.isMainMethod(sym.owner) && sym.owner.is(Synthetic)

    /** If -Wunused:strict-no-implicit-warn import and this import selector could potentially import implicit.
     */
    private def shouldSelectorBeReported(imp: Import, sel: ImportSelector)(using Context): Boolean =
      ctx.settings.WunusedHas.strictNoImplicitWarn && (
        sel.isWildcard ||
        imp.expr.tpe.member(sel.name.toTermName).alternatives.exists(_.symbol.isOneOf(GivenOrImplicit)) ||
        imp.expr.tpe.member(sel.name.toTypeName).alternatives.exists(_.symbol.isOneOf(GivenOrImplicit))
      )

    /** Ignore CanEqual imports.
     */
    private def derivesFromCanEqual(sym: Symbol)(using Context): Boolean =
      sym.isOneOf(GivenOrImplicit) && sym.typeRef.baseClasses.exists(_.derivesFrom(defn.CanEqualClass))

    private def isImportIgnored(imp: Import, sel: ImportSelector)(using Context): Boolean =
      sel.isWildcard && sel.isGiven && imp.expr.tpe.allMembers.exists(p => derivesFromCanEqual(p.symbol))
      ||
      imp.expr.tpe.member(sel.name.toTermName).alternatives.exists(p => derivesFromCanEqual(p.symbol))

    /** Ignore definitions of CanEqual given.
     */
    private def isDefIgnored(memDef: MemberDef)(using Context): Boolean = derivesFromCanEqual(memDef.symbol)

    extension (sel: ImportSelector)
      def boundTpe: Type = sel.bound match
        case untpd.TypedSplice(tree) => tree.tpe
        case _ => NoType
      /** This is used to ignore exclusion imports of the form import `qual`.{`member` => _}
       *  because `sel.isUnimport` is too broad for old style `import concurrent._`.
       */
      def isImportExclusion: Boolean = sel.renamed match
        case untpd.Ident(nme.WILDCARD) => true
        case _ => false

    extension (sym: Symbol)

      /** Given an import selector, is the symbol imported from the given prefix, optionally with a specific name?
       *  If isDerived, then it may be an aliased type in source but we only witness it dealiased.
       */
      private def isInImport(selData: ImportSelectorData, altName: Option[Name], prefix: Type)(using Context): Boolean =
        assert(sym.exists)

        val selector = selData.selector

        if selector.isWildcard then
          selData.qualTpe.member(sym.name).hasAltWith(_.symbol == sym) && { // The qualifier must have the target symbol as a member
            if selector.isGiven then // Further check that the symbol is a given or implicit and conforms to the bound
              sym.isOneOf(Given | Implicit)
                && (selector.bound.isEmpty || sym.info.finalResultType <:< selector.boundTpe)
                && selData.qualTpe =:= prefix
            else
              !sym.is(Given) // Normal wildcard, check that the symbol is not a given (but can be implicit)
          }
        else
          !altName.exists(_.toTermName != selector.rename) && // if there is an explicit name, it must match
            selData.qualTpe =:= prefix && selData.allSymbolsForNamed.contains(sym)
      end isInImport

      /** Annotated with @unused */
      private def hasUnusedAnnot(using Context): Boolean =
        sym.annotations.exists(_.symbol == ctx.definitions.UnusedAnnot)

      private def shouldNotReportParamOwner(using Context): Boolean =
        sym.exists && {
          val owner = sym.owner
          trivialDefs(owner) || // is a trivial def
          owner.isPrimaryConstructor ||
          owner.isDeprecated ||
          owner.isAllOf(Synthetic | PrivateLocal) ||
          owner.is(Accessor) ||
          owner.isOverridden
        }

      private def usedDefContains(using Context): Boolean =
        sym.everySymbol.exists(usedDef.apply)

      private def everySymbol(using Context): List[Symbol] =
        List(sym, sym.companionClass, sym.companionModule, sym.moduleClass).filter(_.exists)

      /** A function is overridden. Either has `override flags` or parent has a matching member (type and name) */
      private def isOverridden(using Context): Boolean =
        sym.is(Flags.Override) || (sym.exists && sym.owner.thisType.parents.exists(p => sym.matchingMember(p).exists))

    end extension

    extension (defdef: DefDef)
      // so trivial that it never consumes params
      private def isTrivial(using Context): Boolean =
        val rhs = defdef.rhs
        rhs.symbol == ctx.definitions.Predef_undefined ||
        rhs.tpe =:= ctx.definitions.NothingType ||
        defdef.symbol.is(Deferred) ||
        rhs.match
          case _: Literal => true
          case _ => rhs.tpe match
            case ConstantType(_) => true
            case tp: TermRef =>
              // Detect Scala 2 SingleType
              tp.underlying.classSymbol.is(Flags.Module)
            case _ =>
              false
      def registerTrivial(using Context): Unit =
        if defdef.isTrivial then
          trivialDefs += defdef.symbol

    extension (memDef: MemberDef)
      private def isValidMemberDef(using Context): Boolean =
        memDef.symbol.exists
          && !memDef.symbol.hasUnusedAnnot
          && !memDef.symbol.isAllOf(Flags.AccessorCreationFlags)
          && !memDef.name.isWildcard
          && !memDef.symbol.owner.is(ExtensionMethod)

      private def isValidParam(using Context): Boolean =
        val sym = memDef.symbol
        (sym.is(Param) || sym.isAllOf(PrivateParamAccessor | Local, butNot = CaseAccessor)) &&
        !isSyntheticMainParam(sym) &&
        !sym.shouldNotReportParamOwner

      private def shouldReportPrivateDef(using Context): Boolean =
        peekScopeType == ScopeType.Template
        && !memDef.symbol.isConstructor
        && memDef.symbol.is(Private, butNot = SelfName | Synthetic | CaseAccessor)
        && !ignoredNames(memDef.symbol.name.toTermName)

      private def isUnsetVarDef(using Context): Boolean =
        val sym = memDef.symbol
        sym.is(Mutable) && !setVars(sym)

    extension (imp: Import)
      /** Enum generate an import for its cases (but outside them), which should be ignored */
      def isGeneratedByEnum(using Context): Boolean =
        imp.symbol.exists && imp.symbol.owner.is(Flags.Enum, butNot = Flags.Case)

    extension (thisName: Name)
      private def isWildcard: Boolean =
        thisName == nme.WILDCARD || thisName.is(WildcardParamName)

  end UnusedData

  private object UnusedData:
    enum ScopeType:
      case Local
      case Template
      case ReplWrapper
      case Other

    object ScopeType:
      /** return the scope corresponding to the enclosing scope of the given tree */
      def fromTree(tree: Tree)(using Context): ScopeType = tree match
        case _: Template => if tree.symbol.name.isReplWrapperName then ReplWrapper else Template
        case _: Block => Local
        case _ => Other

    val ignoredNames: Set[TermName] = Set("readResolve", "readObject", "writeObject", "writeReplace").map(termName(_))

    final case class ImportSelectorData(val qualTpe: Type, val selector: ImportSelector):
      private var myUsed: Boolean = false

      def markUsed(): Unit = myUsed = true

      def isUsed: Boolean = myUsed

      private var myAllSymbols: Set[Symbol] | Null = null

      def allSymbolsForNamed(using Context): Set[Symbol] =
        if myAllSymbols == null then
          val allDenots = qualTpe.member(selector.name).alternatives ::: qualTpe.member(selector.name.toTypeName).alternatives
          myAllSymbols = allDenots.map(_.symbol).toSet
        myAllSymbols.uncheckedNN

    end ImportSelectorData

    case class UnusedSymbol(pos: SrcPos, name: Name, warnType: WarnTypes)
    /** A container for the results of the used elements analysis */
    class UnusedResult(val warnings: Set[UnusedSymbol])
    object UnusedResult:
      val Empty = UnusedResult(Set.empty)

    /** A symbol usage includes the name under which it was observed,
     *  and the prefix from which it was selected.
     */
    case class Usage(val symbol: Symbol, val name: Option[Name], val prefix: Type)
  end UnusedData
  extension (sym: Symbol)
    /** is accessible without import in current context */
    def isAccessibleAsIdent(using Context): Boolean =
      ctx.outersIterator.exists: c =>
        c.owner == sym.owner
        || sym.owner.isClass && c.owner.isClass
            && c.owner.thisType.baseClasses.contains(sym.owner)
            && c.owner.thisType.member(sym.name).alternatives.contains(sym)

  extension (tp: Type)
    def importPrefix(using Context): Type = tp match
      case tp: NamedType => tp.prefix
      case tp: ClassInfo => tp.prefix
      case tp: TypeProxy => tp.superType.normalizedPrefix
      case _ => NoType
    def underlyingPrefix(using Context): Type = tp match
      case tp: NamedType => tp.prefix
      case tp: ClassInfo => tp.prefix
      case tp: TypeProxy => tp.underlying.underlyingPrefix
      case _ => NoType
    def skipPackageObject(using Context): Type =
      if tp.typeSymbol.isPackageObject then tp.underlyingPrefix else tp
    def underlying(using Context): Type = tp match
      case tp: TypeProxy => tp.underlying
      case _ => tp
end CheckUnused
