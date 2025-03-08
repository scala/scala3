package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.desugar.{ForArtifact, PatternVar}
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.ast.untpd, untpd.ImportSelector
import dotty.tools.dotc.config.ScalaSettings
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Names.{Name, SimpleName, DerivedName, TermName, termName}
import dotty.tools.dotc.core.NameOps.{isAnonymousFunctionName, isReplWrapperName}
import dotty.tools.dotc.core.NameKinds.{BodyRetainerName, ContextBoundParamName, ContextFunctionParamName, WildcardParamName}
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.{ClassSymbol, NoSymbol, Symbol, defn, isDeprecated, requiredClass, requiredModule}
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.report
import dotty.tools.dotc.reporting.{CodeAction, UnusedSymbol}
import dotty.tools.dotc.rewrites.Rewrites
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.typer.{ImportInfo, Typer}
import dotty.tools.dotc.typer.Deriving.OriginalTypeClass
import dotty.tools.dotc.util.{Property, Spans, SrcPos}, Spans.Span
import dotty.tools.dotc.util.Chars.{isLineBreakChar, isWhitespace}
import dotty.tools.dotc.util.chaining.*

import java.util.IdentityHashMap

import scala.collection.mutable, mutable.{ArrayBuilder, ListBuffer, Stack}

import CheckUnused.*

/** A compiler phase that checks for unused imports or definitions.
 */
class CheckUnused private (phaseMode: PhaseMode, suffix: String) extends MiniPhase:

  override def phaseName: String = s"checkUnused$suffix"

  override def description: String = "check for unused elements"

  override def isEnabled(using Context): Boolean = ctx.settings.WunusedHas.any

  override def isRunnable(using Context): Boolean = super.isRunnable && ctx.settings.WunusedHas.any && !ctx.isJava

  override def prepareForUnit(tree: Tree)(using Context): Context =
    val infos = tree.getAttachment(refInfosKey).getOrElse:
      RefInfos().tap(tree.withAttachment(refInfosKey, _))
    ctx.fresh.setProperty(refInfosKey, infos)
  override def transformUnit(tree: Tree)(using Context): tree.type =
    if phaseMode == PhaseMode.Report then
      reportUnused()
      tree.removeAttachment(refInfosKey)
    tree

  override def transformIdent(tree: Ident)(using Context): tree.type =
    if tree.symbol.exists then
      // if in an inline expansion, resolve at summonInline (synthetic pos) or in an enclosing call site
      val resolving =
           refInfos.inlined.isEmpty
        || tree.srcPos.isZeroExtentSynthetic
        || refInfos.inlined.exists(_.sourcePos.contains(tree.srcPos.sourcePos))
      if resolving && !ignoreTree(tree) then
        def loopOverPrefixes(prefix: Type, depth: Int): Unit =
          if depth < 10 && prefix.exists && !prefix.classSymbol.isEffectiveRoot then
            resolveUsage(prefix.classSymbol, nme.NO_NAME, NoPrefix)
            loopOverPrefixes(prefix.normalizedPrefix, depth + 1)
        if tree.srcPos.isZeroExtentSynthetic then
          loopOverPrefixes(tree.typeOpt.normalizedPrefix, depth = 0)
        resolveUsage(tree.symbol, tree.name, tree.typeOpt.importPrefix.skipPackageObject)
    else if tree.hasType then
      resolveUsage(tree.tpe.classSymbol, tree.name, tree.tpe.importPrefix.skipPackageObject)
    tree

  // import x.y; y may be rewritten x.y, also import x.z as y
  override def transformSelect(tree: Select)(using Context): tree.type =
    val name = tree.removeAttachment(OriginalName).getOrElse(nme.NO_NAME)
    inline def isImportable = tree.qualifier.srcPos.isSynthetic
      && tree.qualifier.tpe.match
        case ThisType(_) | SuperType(_, _) => false
        case qualtpe => qualtpe.isStable
    if tree.srcPos.isSynthetic && tree.symbol == defn.TypeTest_unapply then
      tree.qualifier.tpe.underlying.finalResultType match
      case AppliedType(tycon, args) =>
        val res =
          if tycon.typeSymbol == defn.TypeTestClass then args(1) // T in TypeTest[-S, T]
          else if tycon.typeSymbol == defn.TypeableType then args(0) // T in Typeable[T]
          else return tree
        val target = res.dealias.typeSymbol
        resolveUsage(target, target.name, res.importPrefix.skipPackageObject) // case _: T =>
      case _ =>
    else if isImportable || name.exists(_ != tree.symbol.name) then
      if !ignoreTree(tree) then
        resolveUsage(tree.symbol, name, tree.qualifier.tpe)
    else if !ignoreTree(tree) then
      refUsage(tree.symbol)
    tree

  override def transformLiteral(tree: Literal)(using Context): tree.type =
    tree.getAttachment(Typer.AdaptedTree).foreach(transformAllDeep)
    tree

  override def prepareForCaseDef(tree: CaseDef)(using Context): Context =
    nowarner.traverse(tree.pat)
    ctx

  override def prepareForApply(tree: Apply)(using Context): Context =
    // ignore tupling of for assignments, as they are not usages of vars
    if tree.hasAttachment(ForArtifact) then
      tree match
      case Apply(TypeApply(Select(fun, nme.apply), _), args) =>
        if fun.symbol.is(Module) && defn.isTupleClass(fun.symbol.companionClass) then
          args.foreach(_.withAttachment(ForArtifact, ()))
      case _ =>
    ctx

  override def prepareForAssign(tree: Assign)(using Context): Context =
    tree.lhs.putAttachment(Ignore, ()) // don't take LHS reference as a read
    ctx
  override def transformAssign(tree: Assign)(using Context): tree.type =
    tree.lhs.removeAttachment(Ignore)
    val sym = tree.lhs.symbol
    if sym.exists then
      refInfos.asss.addOne(sym)
    tree

  override def prepareForMatch(tree: Match)(using Context): Context =
    // allow case.pat against tree.selector (simple var pat only for now)
    tree.selector match
    case Ident(nm) => tree.cases.foreach(k => allowVariableBindings(List(nm), List(k.pat)))
    case _ =>
    ctx
  override def transformMatch(tree: Match)(using Context): tree.type =
    if tree.isInstanceOf[InlineMatch] && tree.selector.isEmpty then
      val sf = defn.Compiletime_summonFrom
      resolveUsage(sf, sf.name, NoPrefix)
    tree

  override def transformTypeTree(tree: TypeTree)(using Context): tree.type =
    tree.tpe match
    case AnnotatedType(_, annot) => transformAllDeep(annot.tree)
    case tpt if !tree.isInferred && tpt.typeSymbol.exists => resolveUsage(tpt.typeSymbol, tpt.typeSymbol.name, NoPrefix)
    case _ =>
    tree

  override def prepareForInlined(tree: Inlined)(using Context): Context =
    refInfos.inlined.push(tree.call.srcPos)
    ctx
  override def transformInlined(tree: Inlined)(using Context): tree.type =
    transformAllDeep(tree.expansion) // traverse expansion with nonempty inlined stack to avoid registering defs
    val _ = refInfos.inlined.pop()
    transformAllDeep(tree.call)
    tree

  override def prepareForBind(tree: Bind)(using Context): Context =
    refInfos.register(tree)
    ctx

  override def prepareForValDef(tree: ValDef)(using Context): Context =
    if !tree.symbol.is(Deferred) && tree.rhs.symbol != defn.Predef_undefined then
      refInfos.register(tree)
    tree.tpt match
    case RefinedTypeTree(_, refinements) => relax(tree.rhs, refinements)
    case _ =>
    ctx
  override def transformValDef(tree: ValDef)(using Context): tree.type =
    traverseAnnotations(tree.symbol)
    if tree.name.startsWith("derived$") && tree.hasType then
      def loop(t: Tree): Unit = t match
        case Ident(name)  => resolveUsage(t.tpe.typeSymbol, name, t.tpe.underlyingPrefix.skipPackageObject)
        case Select(t, _) => loop(t)
        case _            =>
      tree.getAttachment(OriginalTypeClass).foreach(loop)
    if tree.symbol.isAllOf(DeferredGivenFlags) then
      resolveUsage(defn.Compiletime_deferred, nme.NO_NAME, NoPrefix)
    tree

  override def prepareForDefDef(tree: DefDef)(using Context): Context =
    def trivial = tree.symbol.is(Deferred) || isUnconsuming(tree.rhs)
    def nontrivial = tree.symbol.isConstructor || tree.symbol.isAnonymousFunction
    if !nontrivial && trivial then
      refInfos.skip.addOne(tree.symbol)
    if tree.symbol.is(Inline) then
      refInfos.inliners += 1
    else if !tree.symbol.is(Deferred) && tree.rhs.symbol != defn.Predef_undefined then
      refInfos.register(tree)
    tree.tpt match
    case RefinedTypeTree(_, refinements) => relax(tree.rhs, refinements)
    case _ =>
    ctx
  override def transformDefDef(tree: DefDef)(using Context): tree.type =
    traverseAnnotations(tree.symbol)
    if tree.symbol.is(Inline) then
      refInfos.inliners -= 1
    if tree.symbol.isAllOf(DeferredGivenFlags) then
      resolveUsage(defn.Compiletime_deferred, nme.NO_NAME, NoPrefix)
    tree

  override def transformTypeDef(tree: TypeDef)(using Context): tree.type =
    traverseAnnotations(tree.symbol)
    if !tree.symbol.is(Param) then // type parameter to do?
      refInfos.register(tree)
    tree

  override def prepareForTemplate(tree: Template)(using Context): Context =
    ctx.fresh.setProperty(resolvedKey, Resolved())

  override def prepareForPackageDef(tree: PackageDef)(using Context): Context =
    ctx.fresh.setProperty(resolvedKey, Resolved())

  override def prepareForStats(trees: List[Tree])(using Context): Context =
    ctx.fresh.setProperty(resolvedKey, Resolved())

  override def transformOther(tree: Tree)(using Context): tree.type =
    tree match
    case imp: Import =>
      if phaseMode eq PhaseMode.Aggregate then
        refInfos.register(imp)
      transformAllDeep(imp.expr)
      for selector <- imp.selectors do
        if selector.isGiven then
          selector.bound match
          case untpd.TypedSplice(bound) => transformAllDeep(bound)
          case _ =>
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
      // selftype of object is not a usage
      val moduleSelfRef = ctx.owner.is(Module) && ctx.owner == tree.symbol.companionModule.moduleClass
      if !moduleSelfRef then
        transformAllDeep(ref)
    case TypeBoundsTree(lo, hi, alias) =>
      transformAllDeep(lo)
      transformAllDeep(hi)
      transformAllDeep(alias)
    case tree: NamedArg => transformAllDeep(tree.arg)
    case Annotated(arg, annot) =>
      transformAllDeep(arg)
      transformAllDeep(annot)
    case Quote(body, tags) =>
      transformAllDeep(body)
      tags.foreach(transformAllDeep)
    case Splice(expr) =>
      transformAllDeep(expr)
    case QuotePattern(bindings, body, quotes) =>
      bindings.foreach(transformAllDeep)
      transformAllDeep(body)
      transformAllDeep(quotes)
    case SplicePattern(body, typeargs, args) =>
      transformAllDeep(body)
      typeargs.foreach(transformAllDeep)
      args.foreach(transformAllDeep)
    case MatchTypeTree(bound, selector, cases) =>
      transformAllDeep(bound)
      transformAllDeep(selector)
      cases.foreach(transformAllDeep)
    case ByNameTypeTree(result) =>
      transformAllDeep(result)
    //case _: InferredTypeTree => // do nothing
    //case _: Export => // nothing to do
    //case _ if tree.isType =>
    case _ =>
    tree

  private def traverseAnnotations(sym: Symbol)(using Context): Unit =
    for annot <- sym.denot.annotations do
      transformAllDeep(annot.tree)

  // if sym is not an enclosing element, record the reference
  def refUsage(sym: Symbol)(using Context): Unit =
    if !ctx.outersIterator.exists(cur => cur.owner eq sym) then
      refInfos.refs.addOne(sym)

  /** Look up a reference in enclosing contexts to determine whether it was introduced by a definition or import.
   *  The binding of highest precedence must then be correct.
   *
   *  Unqualified locals and fully qualified globals are neither imported nor in scope;
   *  e.g., in `scala.Int`, `scala` is in scope for typer, but here we reverse-engineer the attribution.
   *  For Select, lint does not look up `<empty>.scala` (so top-level syms look like magic) but records `scala.Int`.
   *  For Ident, look-up finds the root import as usual. A competing import is OK because higher precedence.
   */
  def resolveUsage(sym0: Symbol, name: Name, prefix: Type)(using Context): Unit =
    import PrecedenceLevels.*
    val sym = sym0.userSymbol

    def matchingSelector(info: ImportInfo): ImportSelector | Null =
      val qtpe = info.site
      def hasAltMember(nm: Name) = qtpe.member(nm).hasAltWith(_.symbol == sym)
      def loop(sels: List[ImportSelector]): ImportSelector | Null = sels match
        case sel :: sels =>
          val matches =
            if sel.isWildcard then
              // if name is different from sym.name, it must be a rename on import, not a wildcard selector
              !name.exists(_.toTermName != sym.name.toTermName)
              // the qualifier must have the target symbol as a member
              && hasAltMember(sym.name)
              && {
                if sel.isGiven then // Further check that the symbol is a given or implicit and conforms to the bound
                     sym.isOneOf(GivenOrImplicit)
                  && (sel.bound.isEmpty || sym.info.finalResultType <:< sel.boundTpe)
                  && (prefix.eq(NoPrefix) || qtpe =:= prefix)
                else
                  !sym.is(Given) // Normal wildcard, check that the symbol is not a given (but can be implicit)
              }
            else
              // if there is an explicit name, it must match
                 !name.exists(_.toTermName != sel.rename)
              && (prefix.eq(NoPrefix) || qtpe =:= prefix)
              && (hasAltMember(sel.name) || hasAltMember(sel.name.toTypeName))
          if matches then sel else loop(sels)
        case nil => null
      loop(info.selectors)

    def checkMember(ctxsym: Symbol): Boolean =
      ctxsym.isClass && sym.owner.isClass
      && ctxsym.thisType.baseClasses.contains(sym.owner)
      && ctxsym.thisType.member(sym.name).hasAltWith(d => d.containsSym(sym) && !name.exists(_ != d.name))

    // Attempt to cache a result at the given context. Not all contexts bear a cache, including NoContext.
    // If there is already any result for the name and prefix, do nothing.
    def addCached(where: Context, result: Precedence): Unit =
      if where.moreProperties ne null then
        where.property(resolvedKey) match
        case Some(resolved) =>
          resolved.record(sym, name, prefix, result)
        case none =>

    // Avoid spurious NoSymbol and also primary ctors which are never warned about.
    // Selections C.this.toString should be already excluded, but backtopped here for eq, etc.
    if !sym.exists || sym.isPrimaryConstructor || sym.isEffectiveRoot || defn.topClasses(sym.owner) then return

    // Find the innermost, highest precedence. Contexts have no nesting levels but assume correctness.
    // If the sym is an enclosing definition (the owner of a context), it does not count toward usages.
    val isLocal = sym.isLocalToBlock
    var candidate: Context = NoContext
    var cachePoint: Context = NoContext // last context with Resolved cache
    var importer: ImportSelector | Null = null // non-null for import context
    var precedence = NoPrecedence // of current resolution
    var done = false
    var cached = false
    val ctxs = ctx.outersIterator
    while !done && ctxs.hasNext do
      val cur = ctxs.next()
      if cur.owner eq sym then
        addCached(cachePoint, Definition)
        return // found enclosing definition
      else if isLocal then
        if cur.owner eq sym.owner then
          done = true // for local def, just checking that it is not enclosing
      else
        val cachedPrecedence =
          cur.property(resolvedKey) match
          case Some(resolved) =>
            // conservative, cache must be nested below the result context
            if precedence.isNone then
              cachePoint = cur // no result yet, and future result could be cached here
            resolved.hasRecord(sym, name, prefix)
          case none => NoPrecedence
        cached = !cachedPrecedence.isNone
        if cached then
          // if prefer cached precedence, then discard previous result
          if precedence.weakerThan(cachedPrecedence) then
            candidate = NoContext
            importer = null
            cachePoint = cur // actual cache context
            precedence = cachedPrecedence // actual cached precedence
          done = true
        else if cur.isImportContext then
          val sel = matchingSelector(cur.importInfo.nn)
          if sel != null then
            if cur.importInfo.nn.isRootImport then
              if precedence.weakerThan(OtherUnit) then
                precedence = OtherUnit
                candidate = cur
                importer = sel
              done = true
            else if sel.isWildcard then
              if precedence.weakerThan(Wildcard) then
                precedence = Wildcard
                candidate = cur
                importer = sel
            else
              if precedence.weakerThan(NamedImport) then
                precedence = NamedImport
                candidate = cur
                importer = sel
        else if checkMember(cur.owner) then
          if sym.srcPos.sourcePos.source == ctx.source then
            precedence = Definition
            candidate = cur
            importer = null // ignore import in same scope; we can't check nesting level
            done = true
          else if precedence.weakerThan(OtherUnit) then
            precedence = OtherUnit
            candidate = cur
    end while
    // record usage and possibly an import
    refInfos.refs.addOne(sym)
    if candidate != NoContext && candidate.isImportContext && importer != null then
      refInfos.sels.put(importer, ())
    // possibly record that we have performed this look-up
    // if no result was found, take it as Definition (local or rooted head of fully qualified path)
    val adjusted = if precedence.isNone then Definition else precedence
    if !cached && (cachePoint ne NoContext) then
      addCached(cachePoint, adjusted)
    if cachePoint ne ctx then
      addCached(ctx, adjusted) // at this ctx, since cachePoint may be far up the outer chain
  end resolveUsage
end CheckUnused

object CheckUnused:

  enum PhaseMode:
    case Aggregate
    case Report

  val refInfosKey = Property.StickyKey[RefInfos]

  val resolvedKey = Property.Key[Resolved]

  inline def refInfos(using Context): RefInfos = ctx.property(refInfosKey).get

  inline def resolved(using Context): Resolved =
    ctx.property(resolvedKey) match
    case Some(res) => res
    case _ => throw new MatchError("no Resolved for context")

  /** Attachment holding the name of an Ident as written by the user. */
  val OriginalName = Property.StickyKey[Name]

  /** Suppress warning in a tree, such as a patvar name allowed by special convention. */
  val NoWarn = Property.StickyKey[Unit]

  /** Ignore reference. */
  val Ignore = Property.StickyKey[Unit]

  class PostTyper extends CheckUnused(PhaseMode.Aggregate, "PostTyper")

  class PostInlining extends CheckUnused(PhaseMode.Report, "PostInlining")

  class RefInfos:
    val defs = mutable.Set.empty[(Symbol, SrcPos)]    // definitions
    val pats = mutable.Set.empty[(Symbol, SrcPos)]    // pattern variables
    val refs = mutable.Set.empty[Symbol]              // references
    val asss = mutable.Set.empty[Symbol]              // targets of assignment
    val skip = mutable.Set.empty[Symbol]              // methods to skip (don't warn about their params)
    val imps = new IdentityHashMap[Import, Unit]         // imports
    val sels = new IdentityHashMap[ImportSelector, Unit] // matched selectors
    def register(tree: Tree)(using Context): Unit = if inlined.isEmpty then
      tree match
      case imp: Import =>
        if inliners == 0
          && languageImport(imp.expr).isEmpty
          && !imp.isGeneratedByEnum
          && !ctx.outer.owner.name.isReplWrapperName
        then
          imps.put(imp, ())
      case tree: Bind =>
        if !tree.name.isInstanceOf[DerivedName] && !tree.name.is(WildcardParamName) && !tree.hasAttachment(NoWarn) then
          pats.addOne((tree.symbol, tree.namePos))
      case tree: ValDef if tree.hasAttachment(PatternVar) =>
        if !tree.name.isInstanceOf[DerivedName] then
          pats.addOne((tree.symbol, tree.namePos))
      case tree: NamedDefTree =>
        if (tree.symbol ne NoSymbol)
          && !tree.name.isWildcard
          && !tree.hasAttachment(NoWarn)
          && !tree.symbol.is(ModuleVal) // track only the ModuleClass using the object symbol, with correct namePos
        then
          defs.addOne((tree.symbol.userSymbol, tree.namePos))
      case _ =>
        if tree.symbol ne NoSymbol then
          defs.addOne((tree.symbol, tree.srcPos)) // TODO is this a code path

    val inlined = Stack.empty[SrcPos] // enclosing call.srcPos of inlined code (expansions)
    var inliners = 0 // depth of inline def (not inlined yet)
  end RefInfos

  // Symbols already resolved in the given Context (with name and prefix of lookup).
  class Resolved:
    import PrecedenceLevels.*
    private val seen = mutable.Map.empty[Symbol, List[(Name, Type, Precedence)]].withDefaultValue(Nil)
    // if a result has been recorded, return it; otherwise, NoPrecedence.
    def hasRecord(symbol: Symbol, name: Name, prefix: Type)(using Context): Precedence =
      seen(symbol).find((n, p, _) => n == name && p =:= prefix) match
      case Some((_, _, r)) => r
      case none => NoPrecedence
    // "record" the look-up result, if there is not already a result for the name and prefix.
    def record(symbol: Symbol, name: Name, prefix: Type, result: Precedence)(using Context): Unit =
      require(NoPrecedence.weakerThan(result))
      seen.updateWith(symbol):
        case svs @ Some(vs) =>
          if vs.exists((n, p, _) => n == name && p =:= prefix) then svs
          else Some((name, prefix, result) :: vs)
        case none => Some((name, prefix, result) :: Nil)

  // Names are resolved by definitions and imports, which have four precedence levels:
  object PrecedenceLevels:
    opaque type Precedence = Int
    inline def NoPrecedence: Precedence = 5
    inline def OtherUnit: Precedence = 4   // root import or def from another compilation unit via enclosing package
    inline def Wildcard: Precedence = 3    // wildcard import
    inline def NamedImport: Precedence = 2 // specific import
    inline def Definition: Precedence = 1  // def from this compilation unit
    extension (p: Precedence)
      inline def weakerThan(q: Precedence): Boolean = p > q
      inline def isNone: Boolean = p == NoPrecedence

  def reportUnused()(using Context): Unit =
    for (msg, pos, origin) <- warnings do
      if origin.isEmpty then report.warning(msg, pos)
      else report.warning(msg, pos, origin)
      msg.actions.headOption.foreach(Rewrites.applyAction)

  type MessageInfo = (UnusedSymbol, SrcPos, String) // string is origin or empty

  def warnings(using Context): Array[MessageInfo] =
    val actionable = ctx.settings.rewrite.value.nonEmpty
    val warnings = ArrayBuilder.make[MessageInfo]
    def warnAt(pos: SrcPos)(msg: UnusedSymbol, origin: String = ""): Unit = warnings.addOne((msg, pos, origin))
    val infos = refInfos

    def checkUnassigned(sym: Symbol, pos: SrcPos) =
      if sym.isLocalToBlock then
        if ctx.settings.WunusedHas.locals && sym.is(Mutable) && !infos.asss(sym) then
          warnAt(pos)(UnusedSymbol.unsetLocals)
      else if ctx.settings.WunusedHas.privates && sym.isAllOf(Private | Mutable) && !infos.asss(sym) then
        warnAt(pos)(UnusedSymbol.unsetPrivates)

    def checkPrivate(sym: Symbol, pos: SrcPos) =
      if ctx.settings.WunusedHas.privates
        && !sym.isPrimaryConstructor
        && !sym.isOneOf(SelfName | Synthetic | CaseAccessor)
        && !sym.name.is(BodyRetainerName)
        && !sym.isSerializationSupport
        && !(sym.is(Mutable) && sym.isSetter && sym.owner.is(Trait)) // tracks sym.underlyingSymbol sibling getter
      then
        warnAt(pos)(UnusedSymbol.privateMembers)

    def checkParam(sym: Symbol, pos: SrcPos) =
      val m = sym.owner
      def allowed =
        val dd = defn
           m.isDeprecated
        || m.is(Synthetic) && !m.isAnonymousFunction
        || m.hasAnnotation(defn.UnusedAnnot) // param of unused method
        || sym.info.isSingleton
        || m.isConstructor && m.owner.thisType.baseClasses.contains(defn.AnnotationClass)
      def checkExplicit(): Unit =
        // A class param is unused if its param accessor is unused.
        // (The class param is not assigned to a field until constructors.)
        // A local param accessor warns as a param; a private accessor as a private member.
        // Avoid warning for case class elements because they are aliased via unapply.
        if m.isPrimaryConstructor then
          val alias = m.owner.info.member(sym.name)
          if alias.exists then
            val aliasSym = alias.symbol
            if aliasSym.isAllOf(PrivateParamAccessor, butNot = CaseAccessor) && !infos.refs(alias.symbol) then
              if aliasSym.is(Local) then
                if ctx.settings.WunusedHas.explicits then
                  warnAt(pos)(UnusedSymbol.explicitParams)
              else
                if ctx.settings.WunusedHas.privates then
                  warnAt(pos)(UnusedSymbol.privateMembers)
        else if ctx.settings.WunusedHas.explicits
          && !sym.is(Synthetic) // param to setter is unused bc there is no field yet
          && !(sym.owner.is(ExtensionMethod) && {
            m.paramSymss.dropWhile(_.exists(_.isTypeParam)) match
            case (h :: Nil) :: Nil => h == sym // param is the extended receiver
            case _ => false
          })
          && !sym.name.isInstanceOf[DerivedName]
          && !ctx.platform.isMainMethod(m)
        then
          warnAt(pos)(UnusedSymbol.explicitParams)
      end checkExplicit
      // begin
      if !infos.skip(m)
        && !allowed
      then
        checkExplicit()
    end checkParam

    def checkImplicit(sym: Symbol, pos: SrcPos) =
      val m = sym.owner
      def allowed =
        val dd = defn
           m.isDeprecated
        || m.is(Synthetic)
        || sym.name.is(ContextFunctionParamName)    // a ubiquitous parameter
        || sym.name.is(ContextBoundParamName) && sym.info.typeSymbol.isMarkerTrait // a ubiquitous parameter
        || m.hasAnnotation(dd.UnusedAnnot)          // param of unused method
        || sym.info.typeSymbol.match                // more ubiquity
           case dd.DummyImplicitClass | dd.SubTypeClass | dd.SameTypeClass => true
           case _ => false
        || sym.info.isSingleton // DSL friendly
        || sym.isCanEqual
        || sym.info.typeSymbol.hasAnnotation(dd.LanguageFeatureMetaAnnot)
        || sym.info.isInstanceOf[RefinedType] // can't be expressed as a context bound
      if ctx.settings.WunusedHas.implicits
        && !infos.skip(m)
        && !allowed
      then
        if m.isPrimaryConstructor then
          val alias = m.owner.info.member(sym.name)
          if alias.exists then
            val aliasSym = alias.symbol
            val checking =
                 aliasSym.isAllOf(PrivateParamAccessor, butNot = CaseAccessor)
              || aliasSym.isAllOf(Protected | ParamAccessor, butNot = CaseAccessor) && m.owner.is(Given)
            if checking && !infos.refs(alias.symbol) then
              warnAt(pos)(UnusedSymbol.implicitParams)
        else
          warnAt(pos)(UnusedSymbol.implicitParams)

    def checkLocal(sym: Symbol, pos: SrcPos) =
      if ctx.settings.WunusedHas.locals
        && !sym.is(InlineProxy)
        && !sym.isCanEqual
      then
        warnAt(pos)(UnusedSymbol.localDefs)

    def checkPatvars() =
      // convert the one non-synthetic span so all are comparable; filter NoSpan below
      def uniformPos(sym: Symbol, pos: SrcPos): SrcPos =
        if pos.span.isSynthetic then pos else pos.sourcePos.withSpan(pos.span.toSynthetic)
      // patvars in for comprehensions share the pos of where the name was introduced
      val byPos = infos.pats.groupMap(uniformPos(_, _))((sym, pos) => sym)
      for (pos, syms) <- byPos if pos.span.exists && !syms.exists(_.hasAnnotation(defn.UnusedAnnot)) do
        if !syms.exists(infos.refs(_)) then
          if !syms.exists(v => !v.isLocal && !v.is(Private)) then
            warnAt(pos)(UnusedSymbol.patVars)
        else if syms.exists(_.is(Mutable)) then // check unassigned var
          val sym = // recover the original
            if syms.size == 1 then syms.head
            else infos.pats.find((s, p) => syms.contains(s) && !p.span.isSynthetic).map(_._1).getOrElse(syms.head)
          if sym.is(Mutable) && !infos.asss(sym) then
            if sym.isLocalToBlock then
              warnAt(pos)(UnusedSymbol.unsetLocals)
            else if sym.is(Private) then
              warnAt(pos)(UnusedSymbol.unsetPrivates)

    def checkImports() =
      // TODO check for unused masking import
      import scala.jdk.CollectionConverters.given
      import Rewrites.ActionPatch
      type ImpSel = (Import, ImportSelector)
      def isUsable(imp: Import, sel: ImportSelector): Boolean =
        sel.isImportExclusion || infos.sels.containsKey(sel) || imp.isLoose(sel)
      def warnImport(warnable: ImpSel, actions: List[CodeAction] = Nil): Unit =
        val (imp, sel) = warnable
        val msg = UnusedSymbol.imports(actions)
        // example collection.mutable.{Map as MutMap}
        val origin = cpy.Import(imp)(imp.expr, List(sel)).show(using ctx.withoutColors).stripPrefix("import ")
        warnAt(sel.srcPos)(msg, origin)

      if !actionable then
        for imp <- infos.imps.keySet.nn.asScala; sel <- imp.selectors if !isUsable(imp, sel) do
          warnImport(imp -> sel)
      else
        // If the rest of the line is blank, include it in the final edit position. (Delete trailing whitespace.)
        // If for deletion, and the prefix of the line is also blank, then include that, too. (Del blank line.)
        def editPosAt(srcPos: SrcPos, forDeletion: Boolean): SrcPos =
          val start = srcPos.span.start
          val end = srcPos.span.end
          val content = srcPos.sourcePos.source.content()
          val prev = content.lastIndexWhere(c => !isWhitespace(c), end = start - 1)
          val emptyLeft = prev < 0 || isLineBreakChar(content(prev))
          val next = content.indexWhere(c => !isWhitespace(c), from = end)
          val emptyRight = next < 0 || isLineBreakChar(content(next))
          val deleteLine = emptyLeft && emptyRight && forDeletion
          val bump = if (deleteLine) 1 else 0 // todo improve to include offset of next line, endline + 1
          val p0 = srcPos.span
          val p1 = if (next >= 0 && emptyRight) p0.withEnd(next + bump) else p0
          val p2 = if (deleteLine) p1.withStart(prev + 1) else p1
          srcPos.sourcePos.withSpan(p2)
        def actionsOf(actions: (SrcPos, String)*): List[CodeAction] =
          val patches = actions.map((srcPos, replacement) => ActionPatch(srcPos.sourcePos, replacement)).toList
          List(CodeAction(title = "unused import", description = Some("remove import"), patches))
        def replace(editPos: SrcPos)(replacement: String): List[CodeAction] = actionsOf(editPos -> replacement)
        def deletion(editPos: SrcPos): List[CodeAction] = actionsOf(editPos -> "")
        def textFor(impsel: ImpSel): String =
          val (imp, sel) = impsel
          val content = imp.srcPos.sourcePos.source.content()
          def textAt(pos: SrcPos) = String(content.slice(pos.span.start, pos.span.end))
          val qual = textAt(imp.expr.srcPos) // keep original
          val selector = textAt(sel.srcPos)  // keep original
          s"$qual.$selector"                 // don't succumb to vagaries of show
        // begin actionable
        val sortedImps = infos.imps.keySet.nn.asScala
                         .filter(_.srcPos.span.exists) // extra caution
                         .toArray
                         .sortBy(_.srcPos.span.point) // sorted by pos, not sort in place
        var index = 0
        while index < sortedImps.length do
          val nextImport = sortedImps.indexSatisfying(from = index + 1)(_.isPrimaryClause) // next import statement
          if sortedImps.indexSatisfying(from = index, until = nextImport): imp =>
              imp.selectors.exists(!isUsable(imp, _)) // check if any selector in statement was unused
          < nextImport then
            // if no usable selectors in the import statement, delete it entirely.
            // if there is exactly one usable selector, then replace with just that selector (i.e., format it).
            // else for each clause, delete it or format one selector or delete unused selectors.
            // To delete a comma separated item, delete start-to-start, but for last item delete a preceding comma.
            // Reminder that first clause span includes the keyword, so delete point-to-start instead.
            val existing = sortedImps.slice(index, nextImport)
            val (keeping, deleting) = existing.iterator.flatMap(imp => imp.selectors.map(imp -> _)).toList
                                      .partition(isUsable(_, _))
            if keeping.isEmpty then
              val editPos = existing.head.srcPos.sourcePos.withSpan:
                Span(start = existing.head.srcPos.span.start, end = existing.last.srcPos.span.end)
              deleting.init.foreach(warnImport(_))
              warnImport(deleting.last, deletion(editPosAt(editPos, forDeletion = true)))
            else if keeping.lengthIs == 1 then
              val editPos = existing.head.srcPos.sourcePos.withSpan:
                Span(start = existing.head.srcPos.span.start, end = existing.last.srcPos.span.end)
              deleting.init.foreach(warnImport(_))
              val text = s"import ${textFor(keeping.head)}"
              warnImport(deleting.last, replace(editPosAt(editPos, forDeletion = false))(text))
            else
              val lostClauses = existing.iterator.filter(imp => !keeping.exists((i, _) => imp eq i)).toList
              for imp <- lostClauses do
                val actions =
                  if imp == existing.last then
                    val content = imp.srcPos.sourcePos.source.content()
                    val prev = existing.lastIndexWhere(i0 => keeping.exists((i, _) => i == i0))
                    val comma = content.indexOf(',', from = existing(prev).srcPos.span.end)
                    val commaPos = imp.srcPos.sourcePos.withSpan:
                      Span(start = comma, end = existing(prev + 1).srcPos.span.start)
                    val srcPos = imp.srcPos
                    val editPos = srcPos.sourcePos.withSpan: // exclude keyword
                      srcPos.span.withStart(srcPos.span.point)
                    actionsOf(commaPos -> "", editPos -> "")
                  else
                    val impIndex = existing.indexOf(imp)
                    val editPos = imp.srcPos.sourcePos.withSpan: // exclude keyword
                      Span(start = imp.srcPos.span.point, end = existing(impIndex + 1).srcPos.span.start)
                    deletion(editPos)
                imp.selectors.init.foreach(sel => warnImport(imp -> sel))
                warnImport(imp -> imp.selectors.last, actions)
              val singletons = existing.iterator.filter(imp => keeping.count((i, _) => imp eq i) == 1).toList
              var seen = List.empty[Import]
              for impsel <- deleting do
                val (imp, sel) = impsel
                if singletons.contains(imp) then
                  if seen.contains(imp) then
                    warnImport(impsel)
                  else
                    seen ::= imp
                    val editPos = imp.srcPos.sourcePos.withSpan:
                      Span(start = imp.srcPos.span.point, end = imp.srcPos.span.end) // exclude keyword
                    val text = textFor(keeping.find((i, _) => imp eq i).get)
                    warnImport(impsel, replace(editPosAt(editPos, forDeletion = false))(text))
                else if !lostClauses.contains(imp) then
                  val actions =
                    if sel == imp.selectors.last then
                      val content = sel.srcPos.sourcePos.source.content()
                      val prev = imp.selectors.lastIndexWhere(s0 => keeping.exists((_, s) => s == s0))
                      val comma = content.indexOf(',', from = imp.selectors(prev).srcPos.span.end)
                      val commaPos = sel.srcPos.sourcePos.withSpan:
                        Span(start = comma, end = imp.selectors(prev + 1).srcPos.span.start)
                      val editPos = sel.srcPos
                      actionsOf(commaPos -> "", editPos -> "")
                    else
                      val selIndex = imp.selectors.indexOf(sel)
                      val editPos = sel.srcPos.sourcePos.withSpan:
                        sel.srcPos.span.withEnd(imp.selectors(selIndex + 1).srcPos.span.start)
                      deletion(editPos)
                  warnImport(impsel, actions)
          end if
          index = nextImport
        end while

    // begin
    for (sym, pos) <- infos.defs.iterator if !sym.hasAnnotation(defn.UnusedAnnot) do
      if infos.refs(sym) then
        checkUnassigned(sym, pos)
      else if sym.isEffectivelyPrivate then
        checkPrivate(sym, pos)
      else if sym.is(Param, butNot = Given | Implicit) then
        checkParam(sym, pos)
      else if sym.is(Param) then // Given | Implicit
        checkImplicit(sym, pos)
      else if sym.isLocalToBlock then
        checkLocal(sym, pos)

    if ctx.settings.WunusedHas.patvars then
      checkPatvars()

    if ctx.settings.WunusedHas.imports || ctx.settings.WunusedHas.strictNoImplicitWarn then
      checkImports()

    def sortOrder(msgInfo: MessageInfo): Int =
      val srcPos = msgInfo._2
      if srcPos.span.exists then srcPos.span.point else 0

    warnings.result().sortBy(sortOrder)
  end warnings

  // Specific exclusions
  def ignoreTree(tree: Tree): Boolean =
    tree.hasAttachment(ForArtifact) || tree.hasAttachment(Ignore)

  // The RHS of a def is too trivial to warn about unused params, e.g. def f(x: Int) = ???
  def isUnconsuming(rhs: Tree)(using Context): Boolean =
        rhs.symbol == defn.Predef_undefined
     || rhs.tpe =:= defn.NothingType // compiletime.error
     || rhs.isInstanceOf[Literal] // 42
     || rhs.tpe.match
        case ConstantType(_) => true
        case tp: TermRef => tp.underlying.classSymbol.is(Module) // Scala 2 SingleType
        case _ => false
     //|| isPurePath(rhs) // a bit strong
     || rhs.match
        case Block((dd @ DefDef(anonfun, paramss, _, _)) :: Nil, Closure(Nil, Ident(nm), _)) =>
             anonfun == nm // isAnonymousFunctionName(anonfun)
          && paramss.match
             case (ValDef(contextual, _, _) :: Nil) :: Nil =>
                  contextual.is(ContextFunctionParamName)
               && isUnconsuming(dd.rhs) // rhs was wrapped in a context function
             case _ => false
        case Block(Nil, Literal(u)) => u.tpe =:= defn.UnitType // def f(x: X) = {}
        case This(_) => true
        case Ident(_) => rhs.symbol.is(ParamAccessor)
        case Typed(rhs, _) => isUnconsuming(rhs)
        case _ => false

  def allowVariableBindings(ok: List[Name], args: List[Tree]): Unit =
    ok.zip(args).foreach:
      case (param, arg @ Bind(p, _)) if param == p => arg.withAttachment(NoWarn, ())
      case _ =>

  // NoWarn Binds if the name matches a "canonical" name, e.g. case element name
  val nowarner = new TreeTraverser:
    def traverse(tree: Tree)(using Context) = tree match
      case UnApply(fun, _, args) =>
        val unapplied = tree.tpe.finalResultType.dealias.typeSymbol
        if unapplied.is(CaseClass) then
          allowVariableBindings(unapplied.primaryConstructor.info.firstParamNames, args)
        else if fun.symbol == defn.PairClass_unapply then
          val ok = fun.symbol.info match
            case PolyType(tycon, MethodTpe(_, _, AppliedType(_, tprefs))) =>
              tprefs.collect:
                case ref: TypeParamRef => termName(ref.binder.paramNames(ref.paramNum).toString.toLowerCase.nn)
            case _ => Nil
          allowVariableBindings(ok, args)
        else if fun.symbol == defn.TypeTest_unapply then
          () // just recurse into args
        else
          if unapplied.exists && unapplied.owner == defn.Quotes_reflectModule then
            // cheapy search for parameter names via java reflection of Trees
            // in lieu of drilling into requiredClass("scala.quoted.runtime.impl.QuotesImpl")
            // ...member("reflect")...member(unapplied.name.toTypeName)
            // with aliases into requiredModule("dotty.tools.dotc.ast.tpd")
            val implName = s"dotty.tools.dotc.ast.Trees$$${unapplied.name}"
            try
              import scala.language.unsafeNulls
              val clz = Class.forName(implName) // TODO improve to use class path or reflect
              val ok = clz.getConstructors.head.getParameters.map(p => termName(p.getName)).toList.init
              allowVariableBindings(ok, args)
            catch case _: ClassNotFoundException => ()
        args.foreach(traverse)
      case tree => traverseChildren(tree)

  // NoWarn members in tree that correspond to refinements; currently uses only names.
  def relax(tree: Tree, refinements: List[Tree])(using Context): Unit =
    val names = refinements.collect { case named: NamedDefTree => named.name }.toSet
    val relaxer = new TreeTraverser:
      def traverse(tree: Tree)(using Context) =
        tree match
        case tree: NamedDefTree if names(tree.name) => tree.withAttachment(NoWarn, ())
        case _ =>
        traverseChildren(tree)
    relaxer.traverse(tree)

  extension (nm: Name)
    inline def exists(p: Name => Boolean): Boolean = nm.ne(nme.NO_NAME) && p(nm)
    inline def isWildcard: Boolean = nm == nme.WILDCARD || nm.is(WildcardParamName)

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

  private val serializationNames: Set[TermName] =
    Set("readResolve", "readObject", "readObjectNoData", "writeObject", "writeReplace").map(termName(_))

  extension (sym: Symbol)
    def isSerializationSupport(using Context): Boolean =
      sym.is(Method) && serializationNames(sym.name.toTermName) && sym.owner.isClass
        && sym.owner.derivesFrom(defn.JavaSerializableClass)
    def isCanEqual(using Context): Boolean =
      sym.isOneOf(GivenOrImplicit) && sym.info.finalResultType.baseClasses.exists(_.derivesFrom(defn.CanEqualClass))
    def isMarkerTrait(using Context): Boolean =
      sym.isClass && sym.info.allMembers.forall: d =>
        val m = d.symbol
        !m.isTerm || m.isSelfSym || m.is(Method) && (m.owner == defn.AnyClass || m.owner == defn.ObjectClass)
    def isEffectivelyPrivate(using Context): Boolean =
      sym.is(Private, butNot = ParamAccessor)
      || sym.owner.isAnonymousClass && !sym.nextOverriddenSymbol.exists
    // pick the symbol the user wrote for purposes of tracking
    inline def userSymbol(using Context): Symbol=
      if sym.denot.is(ModuleClass) then sym.denot.companionModule else sym

  extension (sel: ImportSelector)
    def boundTpe: Type = sel.bound match
      case untpd.TypedSplice(tree) => tree.tpe
      case _ => NoType
    /** This is used to ignore exclusion imports of the form import `qual.member as _`
     *  because `sel.isUnimport` is too broad for old style `import concurrent._`.
     */
    def isImportExclusion: Boolean = sel.renamed match
      case untpd.Ident(nme.WILDCARD) => true
      case _ => false

  extension (imp: Import)
    /** Is it the first import clause in a statement? `a.x` in `import a.x, b.{y, z}` */
    def isPrimaryClause(using Context): Boolean =
      imp.srcPos.span.pointDelta > 0 // primary clause starts at `import` keyword with point at clause proper

    /** Generated import of cases from enum companion. */
    def isGeneratedByEnum(using Context): Boolean =
      imp.symbol.exists && imp.symbol.owner.is(Enum, butNot = Case)

    /** Under -Wunused:strict-no-implicit-warn, avoid false positives
     *  if this selector is a wildcard that might import implicits or
     *  specifically does import an implicit.
     *  Similarly, import of CanEqual must not warn, as it is always witness.
     */
    def isLoose(sel: ImportSelector)(using Context): Boolean =
      if ctx.settings.WunusedHas.strictNoImplicitWarn then
        if sel.isWildcard
          || imp.expr.tpe.member(sel.name.toTermName).hasAltWith(_.symbol.isOneOf(GivenOrImplicit))
          || imp.expr.tpe.member(sel.name.toTypeName).hasAltWith(_.symbol.isOneOf(GivenOrImplicit))
        then return true
      if sel.isWildcard && sel.isGiven
      then imp.expr.tpe.allMembers.exists(_.symbol.isCanEqual)
      else imp.expr.tpe.member(sel.name.toTermName).hasAltWith(_.symbol.isCanEqual)

  extension (pos: SrcPos)
    def isZeroExtentSynthetic: Boolean = pos.span.isSynthetic && pos.span.isZeroExtent
    def isSynthetic: Boolean = pos.span.isSynthetic && pos.span.exists

  extension [A <: AnyRef](arr: Array[A])
    // returns `until` if not satisfied
    def indexSatisfying(from: Int, until: Int = arr.length)(p: A => Boolean): Int =
      var i = from
      while i < until && !p(arr(i)) do
        i += 1
      i
end CheckUnused
