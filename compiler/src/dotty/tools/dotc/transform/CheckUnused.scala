package dotty.tools.dotc
package transform

import ast.*, desugar.{ForArtifact, PatternVar}, tpd.*, untpd.ImportSelector
import config.ScalaSettings
import core.*, Contexts.*, Decorators.*, Flags.*
import Names.{Name, SimpleName, DerivedName, TermName, termName}
import NameKinds.{BodyRetainerName, ContextBoundParamName, ContextFunctionParamName, DefaultGetterName, WildcardParamName}
import NameOps.{isAnonymousFunctionName, isReplWrapperName, setterName}
import Scopes.newScope
import StdNames.nme
import Symbols.{ClassSymbol, NoSymbol, Symbol, defn, isDeprecated, requiredClass, requiredModule}
import Types.*
import reporting.{CodeAction, Diagnostic, UnusedSymbol}
import rewrites.Rewrites.ActionPatch

import MegaPhase.MiniPhase
import typer.{ImportInfo, Typer, TyperPhase}
import typer.Deriving.OriginalTypeClass
import typer.Implicits.{ContextualImplicits, RenamedImplicitRef}
import util.{Property, Spans, SrcPos}, Spans.Span
import util.Chars.{isLineBreakChar, isWhitespace}
import util.chaining.*

import java.util.IdentityHashMap

import scala.collection.mutable, mutable.{ArrayBuilder, ListBuffer, Stack}

import CheckUnused.*

/** A compiler phase that checks for unused imports or definitions.
 */
class CheckUnused private (phaseMode: PhaseMode, suffix: String) extends MiniPhase:

  override def phaseName: String = s"checkUnused$suffix"

  override def description: String = "check for unused elements"

  override def runsAfter = Set:
    phaseMode match
    case PhaseMode.Aggregate => TyperPhase.name
    case PhaseMode.Resolve => Inlining.name
    case PhaseMode.Report => PatternMatcher.name

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
    refInfos.isAssignment = tree.hasAttachment(AssignmentTarget)
    if tree.symbol.exists then
      // if in an inline expansion, resolve at summonInline (synthetic pos) or in an enclosing call site
      val resolving =
           tree.srcPos.isUserCode(using if tree.hasAttachment(InlinedParameter) then ctx.outer else ctx)
        || tree.srcPos.isZeroExtentSynthetic // take as summonInline
      if !ignoreTree(tree) then
        def loopOverPrefixes(prefix: Type, depth: Int): Unit =
          if depth < 10 && prefix.exists && !prefix.classSymbol.isEffectiveRoot then
            resolveUsage(prefix.classSymbol, nme.NO_NAME, NoPrefix, imports = resolving)
            loopOverPrefixes(prefix.normalizedPrefix, depth + 1)
        if tree.srcPos.isZeroExtentSynthetic then
          loopOverPrefixes(tree.typeOpt.normalizedPrefix, depth = 0)
        resolveUsage(tree.symbol, tree.name, tree.typeOpt.importPrefix.skipPackageObject, imports = resolving)
    else if tree.hasType then
      resolveUsage(tree.tpe.classSymbol, tree.name, tree.tpe.importPrefix.skipPackageObject)
    refInfos.isAssignment = false
    tree

  // import x.y; y may be rewritten x.y, also import x.z as y
  override def transformSelect(tree: Select)(using Context): tree.type =
    refInfos.isAssignment = tree.hasAttachment(AssignmentTarget)
    val name = tree.removeAttachment(OriginalName).getOrElse(nme.NO_NAME)
    inline def isImportable = tree.qualifier.srcPos.isSynthetic
      && tree.qualifier.tpe.match
        case ThisType(_) | SuperType(_, _) => false
        case qualtpe => qualtpe.isStable
    val sym = tree.symbol
            .orElse:
              tree.typeOpt.resultType.typeSymbol
    if tree.srcPos.isSynthetic && sym == defn.TypeTest_unapply then
      tree.qualifier.tpe.underlying.finalResultType match
      case AppliedType(tycon, args) =>
        val res =
          if tycon.typeSymbol == defn.TypeTestClass then args(1) // T in TypeTest[-S, T]
          else if tycon.typeSymbol == defn.TypeableType then args(0) // T in Typeable[T]
          else return tree
        val target = res.dealias.typeSymbol
        resolveUsage(target, target.name, res.importPrefix.skipPackageObject) // case _: T =>
      case _ =>
    else if isImportable || name.exists(_ != sym.name) then
      if !ignoreTree(tree) then
        resolveUsage(sym, name, tree.qualifier.tpe)
    else if !ignoreTree(tree) then
      refUsage(sym)
    refInfos.isAssignment = false
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
    else if !tree.tpe.isInstanceOf[MethodOrPoly] then
      val f = funPart(tree)
      if f.symbol.isConstructor then
        f match
        case Select(_: New, nme.CONSTRUCTOR) if ctx.outersIterator.exists(_.owner eq f.symbol.owner) =>
          ignoreArgsOfSelfConstruction(tree, f.symbol)
        case _ =>
    ctx
  override def transformApply(tree: Apply)(using Context): tree.type =
    // check for multiversal equals
    tree match
    case Apply(Select(left, nme.Equals | nme.NotEquals), right :: Nil) =>
      val caneq = defn.CanEqualClass.typeRef.appliedTo(left.tpe.widen :: right.tpe.widen :: Nil)
      resolveScoped(caneq)
    case tree =>
      refUsage(tree.tpe.typeSymbol)
    tree

  override def transformTypeApply(tree: TypeApply)(using Context): tree.type =
    if tree.symbol.exists && tree.symbol.isConstructor then
      refUsage(tree.symbol.owner) // redundant with use of resultType in transformSelect of fun
    tree

  override def prepareForAssign(tree: Assign)(using Context): Context =
    tree.lhs.putAttachment(AssignmentTarget, ()) // don't take LHS reference as a read
    ctx
  override def transformAssign(tree: Assign)(using Context): tree.type =
    tree.lhs.removeAttachment(AssignmentTarget)
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
    if tree.inlinedFromOuterScope then
      tree.expansion.putAttachment(InlinedParameter, ())
    ctx
  override def transformInlined(tree: Inlined)(using Context): tree.type =
    if !tree.call.isEmpty then
      if !refInfos.calls.containsKey(tree.call) then
        refInfos.calls.put(tree.call, ())
        transformAllDeep(tree.call)
    tree

  override def prepareForBind(tree: Bind)(using Context): Context =
    register(tree)
    ctx
  /* cf QuotePattern
  override def transformBind(tree: Bind)(using Context): tree.type =
    tree.symbol.info match
    case TypeBounds(lo, hi) =>
      def resolve(tpe: Type): Unit =
        val sym = tpe.typeSymbol
        if sym.exists then
          resolveUsage(sym, sym.name, NoPrefix)
      resolve(lo)
      resolve(hi)
    case _ =>
    tree
  */

  override def prepareForValDef(tree: ValDef)(using Context): Context =
    if !tree.symbol.is(Deferred) && tree.rhs.symbol != defn.Predef_undefined then
      register(tree)
    relax(tree.rhs, tree.tpt.tpe)
    if tree.symbol.isAllOf(EnumCase) then ctx.outer else ctx
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
    def isDefault = tree.symbol.name.is(DefaultGetterName)
    if !nontrivial && trivial || isDefault then
      refInfos.skip.addOne(tree.symbol)
    if tree.symbol.is(Inline) then
      refInfos.inliners += 1
    else if !tree.symbol.is(Deferred) && tree.rhs.symbol != defn.Predef_undefined then
      register(tree)
    relax(tree.rhs, tree.tpt.tpe)
    ctx
  override def transformDefDef(tree: DefDef)(using Context): tree.type =
    traverseAnnotations(tree.symbol)
    if tree.symbol.is(Inline) then
      refInfos.inliners -= 1
    if tree.symbol.isAllOf(DeferredGivenFlags) then
      resolveUsage(defn.Compiletime_deferred, nme.NO_NAME, NoPrefix)
    tree

  override def prepareForTypeDef(tree: TypeDef)(using Context): Context =
    if tree.symbol.isAllOf(EnumCase) then ctx.outer else ctx
  override def transformTypeDef(tree: TypeDef)(using Context): tree.type =
    traverseAnnotations(tree.symbol)
    if !tree.symbol.is(Param) then // type parameter to do?
      register(tree)
    tree

  override def prepareForStats(trees: List[Tree])(using Context): Context =
    // gather local implicits while ye may
    if !ctx.owner.isClass then
      if trees.exists(t => t.isDef && t.symbol.is(Given) && t.symbol.isLocalToBlock) then
        val scope = newScope.openForMutations
        for tree <- trees if tree.isDef && tree.symbol.is(Given) do
          scope.enter(tree.symbol.name, tree.symbol)
        return ctx.fresh.setScope(scope)
    ctx

  override def transformOther(tree: Tree)(using Context): tree.type =
    tree match
    case imp: Import =>
      register(imp)
      transformAllDeep(imp.expr)
      for selector <- imp.selectors do
        if selector.isGiven then
          selector.bound match
          case untpd.TypedSplice(bound) => transformAllDeep(bound)
          case _ =>
    case exp: Export =>
      transformAllDeep(exp.expr)
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
      bindings.foreach:
        case b @ Bind(_, _) =>
          b.symbol.info match
          case TypeBounds(lo, hi) =>
            def resolve(tpe: Type): Unit =
              val sym = tpe.typeSymbol
              if sym.exists then
                resolveUsage(sym, sym.name, NoPrefix)
            resolve(lo)
            resolve(hi)
          case _ =>
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
    //case _ if tree.isType =>
    case _ =>
    tree

  private def traverseAnnotations(sym: Symbol)(using Context): Unit =
    for annot <- sym.denot.annotations do
      transformAllDeep(annot.tree)

  /** If sym is not an enclosing element with respect to the give context, record the reference
   *
   *  Also check that every enclosing element is not a synthetic member
   *  of the sym's case class companion module.
   */
  def refUsage(sym: Symbol)(using Context): Unit =
    if !refInfos.hasRef(sym) then
      val isCase = sym.is(Case) && sym.isClass
      if !ctx.outersIterator.exists: outer =>
        val owner = outer.owner
           owner.eq(sym)
        || isCase
           && owner.exists
           && owner.is(Synthetic)
           && owner.owner.eq(sym.companionModule.moduleClass)
      then
        refInfos.addRef(sym)

  /** Look up a reference in enclosing contexts to determine whether it was introduced by a definition or import.
   *  The binding of highest precedence must then be correct.
   *
   *  Unqualified locals and fully qualified globals are neither imported nor in scope;
   *  e.g., in `scala.Int`, `scala` is in scope for typer, but here we reverse-engineer the attribution.
   *  For Select, lint does not look up `<empty>.scala` (so top-level syms look like magic) but records `scala.Int`.
   *  For Ident, look-up finds the root import as usual. A competing import is OK because higher precedence.
   *
   *  The `imports` flag is whether an identifier can mark an import as used: the flag is false
   *  for inlined code, except for `summonInline` (and related constructs) which are resolved at inlining.
   */
  def resolveUsage(sym0: Symbol, name: Name, prefix: Type, imports: Boolean = true)(using Context): Unit =
    import PrecedenceLevels.*
    val sym = sym0.userSymbol

    def matchingSelector(info: ImportInfo): ImportSelector | Null =
      val qtpe = info.site
      def hasAltMember(nm: Name) = qtpe.member(nm).hasAltWith: alt =>
        val sameSym =
             alt.symbol == sym
          || nm.isTypeName && alt.symbol.isAliasType && alt.info.dealias.typeSymbol == sym
        sameSym && alt.symbol.isAccessibleFrom(qtpe)
      def hasAltMemberNamed(nm: Name) = qtpe.member(nm).hasAltWith(_.symbol.isAccessibleFrom(qtpe))

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
            else if sel.isUnimport then
              val masksMatchingMember =
                   name != nme.NO_NAME
                && sels.exists(x => x.isWildcard && !x.isGiven)
                && !name.exists(_.toTermName != sel.name) // import a.b as _, b must match name
                && (hasAltMemberNamed(sel.name) || hasAltMemberNamed(sel.name.toTypeName))
              if masksMatchingMember then
                refInfos.sels.put(sel, ()) // imprecise due to precedence but errs on the side of false negative
              false
            else
                 !name.exists(_.toTermName != sel.rename) // if there is an explicit name, it must match
              && (prefix.eq(NoPrefix) || qtpe =:= prefix)
              && (hasAltMember(sel.name) || hasAltMember(sel.name.toTypeName))
          if matches then sel else loop(sels)
        case nil => null
      loop(info.selectors)
    end matchingSelector

    def checkMember(ctxsym: Symbol): Boolean =
      ctxsym.isClass && sym.owner.isClass
      && ctxsym.thisType.baseClasses.contains(sym.owner)
      && ctxsym.thisType.member(sym.name).hasAltWith(d => d.containsSym(sym) && !name.exists(_ != d.name))

    // Avoid spurious NoSymbol and also primary ctors which are never warned about.
    // Selections C.this.toString should be already excluded, but backstopped here for eq, etc.
    if !sym.exists || sym.isPrimaryConstructor || sym.isEffectiveRoot || defn.topClasses(sym.owner) then return

    // Find the innermost, highest precedence. Contexts have no nesting levels but assume correctness.
    // If the sym is an enclosing definition (the owner of a context), it does not count toward usages.
    val isLocal = sym.isLocalToBlock
    var candidate: Context = NoContext
    var importer: ImportSelector | Null = null // non-null for import context
    var precedence = NoPrecedence // of current resolution
    var enclosed = false // true if sym is owner of an enclosing context
    var done = false
    val ctxs = ctx.outersIterator
    while !done && ctxs.hasNext do
      val cur = ctxs.next()
      if cur.owner.userSymbol == sym && !sym.is(Package) then
        enclosed = true // found enclosing definition, don't record the reference
      if cur.isImportContext then
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
      else if isLocal then
        if cur.owner eq sym.owner then
          done = true // local def or param
      else if checkMember(cur.owner) then
        if sym.is(Package) || sym.srcPos.sourcePos.source == ctx.source then
          precedence = Definition
          candidate = cur
          importer = null // ignore import in same scope; we can't check nesting level
          done = true
        else if precedence.weakerThan(OtherUnit) then
          precedence = OtherUnit
          candidate = cur
    end while
    // record usage and possibly an import
    if !enclosed then
      refUsage(sym)
    if imports && candidate != NoContext && candidate.isImportContext && importer != null then
      refInfos.sels.put(importer, ())
  end resolveUsage

  /** Simulate implicit search for contextual implicits in lexical scope and mark any definitions or imports as used.
   *  Avoid cached ctx.implicits because it needs the precise import context that introduces the given.
   */
  def resolveScoped(tp: Type)(using Context): Unit =
    var done = false
    val ctxs = ctx.outersIterator
    while !done && ctxs.hasNext do
      val cur = ctxs.next()
      val implicitRefs: List[ImplicitRef] =
        if (cur.isClassDefContext) cur.owner.thisType.implicitMembers
        else if (cur.isImportContext) cur.importInfo.nn.importedImplicits
        else if (cur.isNonEmptyScopeContext) cur.scope.implicitDecls
        else Nil
      implicitRefs.find(ref => ref.underlyingRef.widen <:< tp) match
      case Some(found: TermRef) =>
        refUsage(found.denot.symbol)
        if cur.isImportContext then
          cur.importInfo.nn.selectors.find(sel => sel.isGiven || sel.rename == found.name) match
          case Some(sel) =>
            refInfos.sels.put(sel, ())
          case _ =>
        return
      case Some(found: RenamedImplicitRef) if cur.isImportContext =>
        refUsage(found.underlyingRef.denot.symbol)
        cur.importInfo.nn.selectors.find(sel => sel.rename == found.implicitName) match
        case Some(sel) =>
          refInfos.sels.put(sel, ())
        case _ =>
        return
      case _ =>
  end resolveScoped

  /** Register new element for warnings only at typer */
  def register(tree: Tree)(using Context): Unit =
    if phaseMode eq PhaseMode.Aggregate then
      refInfos.register(tree)

end CheckUnused

object CheckUnused:

  enum PhaseMode:
    case Aggregate // new defs are considered
    case Resolve   // no new defs, only additional references
    case Report    // report when done

  val refInfosKey = Property.StickyKey[RefInfos]

  inline def refInfos(using Context): RefInfos = ctx.property(refInfosKey).get

  /** Attachment holding the name of an Ident as written by the user. */
  val OriginalName = Property.StickyKey[Name]

  /** Suppress warning in a tree, such as a patvar name allowed by special convention. */
  val NoWarn = Property.StickyKey[Unit]

  /** Ignore reference. */
  val Ignore = Property.StickyKey[Unit]

  /** Tree is LHS of Assign. */
  val AssignmentTarget = Property.StickyKey[Unit]

  /** Tree is an inlined parameter. */
  val InlinedParameter = Property.StickyKey[Unit]

  def PostTyper() = CheckUnused(PhaseMode.Aggregate, "PostTyper")

  def PostInlining() = CheckUnused(PhaseMode.Resolve, "PostInlining")

  def PostPatMat() = CheckUnused(PhaseMode.Report, "PostPatMat")

  class RefInfos:
    val defs = mutable.Set.empty[(Symbol, SrcPos)]    // definitions
    val pats = mutable.Set.empty[(Symbol, SrcPos)]    // pattern variables
    val refs = mutable.Set.empty[Symbol]              // references
    val asss = mutable.Set.empty[Symbol]              // targets of assignment
    val skip = mutable.Set.empty[Symbol]              // methods to skip (don't warn about their params)
    val nowarn = mutable.Set.empty[Symbol]            // marked @nowarn
    val calls = new IdentityHashMap[Tree, Unit]          // inlined call already seen
    val imps = new IdentityHashMap[Import, Unit]         // imports
    val sels = new IdentityHashMap[ImportSelector, Unit] // matched selectors
    def register(tree: Tree)(using Context): Unit = if tree.srcPos.isUserCode then
      tree match
      case imp: Import =>
        if inliners == 0
          && languageImport(imp.expr).isEmpty
          && !imp.isGeneratedByEnum
          && !ctx.owner.name.isReplWrapperName
        then
          if imp.isCompiletimeTesting then
            isNullified = true
          else
            imps.put(imp, ())
      case tree: Bind =>
        if !tree.name.isInstanceOf[DerivedName] && !tree.name.is(WildcardParamName) then
          if tree.hasAttachment(NoWarn) then
            nowarn.addOne(tree.symbol)
          pats.addOne((tree.symbol, tree.namePos))
      case tree: NamedDefTree =>
        if tree.hasAttachment(PatternVar) then
          if !tree.name.isInstanceOf[DerivedName] then
            pats.addOne((tree.symbol, tree.namePos))
        else if (tree.symbol ne NoSymbol)
          && !tree.name.isWildcard
          && !tree.symbol.is(ModuleVal) // track only the ModuleClass using the object symbol, with correct namePos
        then
          if tree.hasAttachment(NoWarn) then
            nowarn.addOne(tree.symbol)
          defs.addOne((tree.symbol.userSymbol, tree.namePos))
      case _ =>
        if tree.symbol ne NoSymbol then
          defs.addOne((tree.symbol, tree.srcPos)) // TODO is this a code path

    var inliners = 0 // depth of inline def (not inlined yet)

    // instead of refs.addOne, use refUsage -> addRef to distinguish a read from a write to var
    var isAssignment = false
    def addRef(sym: Symbol): Unit =
      if isAssignment then
        asss.addOne(sym)
      else
        refs.addOne(sym)
    def hasRef(sym: Symbol): Boolean =
      if isAssignment then
        asss(sym)
      else
        refs(sym)

    // currently compiletime.testing is completely erased, so ignore the unit
    var isNullified = false
  end RefInfos

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

  def reportUnused()(using Context): Unit = if !refInfos.isNullified then
    atPhaseBeforeTransforms:
      for (msg, pos, origin) <- warnings do
        report.warning(msg, pos, origin)

  type MessageInfo = (UnusedSymbol, SrcPos, String) // string is origin or empty

  def warnings(using Context): Array[MessageInfo] =
    val actionable: true = true //ctx.settings.rewrite.value.nonEmpty
    val warnings = ArrayBuilder.make[MessageInfo]
    def warnAt(pos: SrcPos)(msg: UnusedSymbol, origin: String = Diagnostic.OriginWarning.NoOrigin): Unit =
      warnings.addOne((msg, pos, origin))
    val infos = refInfos

    // non-local sym was target of assignment or has a sibling setter that was referenced
    def isMutated(sym: Symbol): Boolean =
         infos.asss(sym)
      || infos.refs(sym.owner.info.member(sym.name.asTermName.setterName).symbol)

    def checkUnassigned(sym: Symbol, pos: SrcPos) =
      if sym.isLocalToBlock then
        if ctx.settings.WunusedHas.locals && sym.is(Mutable) && !infos.asss(sym) then
          warnAt(pos)(UnusedSymbol.unsetLocals)
      else if ctx.settings.WunusedHas.privates
        && sym.is(Mutable)
        && (sym.is(Private) || sym.isEffectivelyPrivate)
        && !sym.isSetter // tracks sym.underlyingSymbol sibling getter, check setter below
        && !isMutated(sym)
      then
        warnAt(pos)(UnusedSymbol.unsetPrivates)

    def checkPrivate(sym: Symbol, pos: SrcPos) =
      if ctx.settings.WunusedHas.privates
        && !sym.isPrimaryConstructor
        && !sym.isOneOf(SelfName | Synthetic | CaseAccessor)
        && !sym.name.is(BodyRetainerName)
        && !sym.isSerializationSupport
        && !( sym.is(Mutable)
           && sym.isSetter // tracks sym.underlyingSymbol sibling getter
           && (sym.owner.is(Trait) || sym.owner.isAnonymousClass)
        )
        && !infos.nowarn(sym)
      then
        if sym.is(Mutable) && isMutated(sym) then
          warnAt(pos)(UnusedSymbol.privateVars)
        else
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
        // Avoid warning for case class elements because they are aliased via unapply (i.e. may be extracted).
        if m.isPrimaryConstructor then
          val alias = m.owner.info.member(sym.name)
          if alias.exists then
            val aliasSym = alias.symbol
            if aliasSym.isAllOf(PrivateParamAccessor, butNot = CaseAccessor)
              && !infos.refs(alias.symbol)
              && !usedByDefaultGetter(sym, m)
            then
              if aliasSym.is(Local) then
                if ctx.settings.WunusedHas.explicits then
                  warnAt(pos)(UnusedSymbol.explicitParams(aliasSym))
              else
                if ctx.settings.WunusedHas.privates then
                  warnAt(pos)(UnusedSymbol.privateMembers)
        else if ctx.settings.WunusedHas.explicits
          && !sym.is(Synthetic) // param to setter is unused bc there is no field yet
          && !(sym.owner.is(ExtensionMethod) &&
            m.paramSymss.dropWhile(_.exists(_.isTypeParam)).match
            case (h :: Nil) :: _ => h == sym // param is the extended receiver
            case _ => false
          )
          && !sym.name.isInstanceOf[DerivedName]
          && !ctx.platform.isMainMethod(m)
          && !usedByDefaultGetter(sym, m)
        then
          warnAt(pos)(UnusedSymbol.explicitParams(sym))
      end checkExplicit
      // begin
      if !infos.skip(m)
        && !m.isEffectivelyOverride
        && !allowed
      then
        checkExplicit()
    end checkParam

    // does the param have an alias in a default arg method that is used?
    def usedByDefaultGetter(param: Symbol, meth: Symbol): Boolean =
      val cls = if meth.isConstructor then meth.enclosingClass.companionModule else meth.enclosingClass
      val MethName = meth.name
      cls.info.decls.exists: d =>
        d.name match
        case DefaultGetterName(MethName, _) =>
          d.paramSymss.exists(_.exists(p => p.name == param.name && infos.refs(p)))
        case _ => false

    def checkImplicit(sym: Symbol, pos: SrcPos) =
      val m = sym.owner
      def allowed =
        val dd = defn
           m.isDeprecated
        || m.is(Synthetic)
        || m.hasAnnotation(dd.UnusedAnnot)          // param of unused method
        || sym.name.is(ContextFunctionParamName)    // a ubiquitous parameter
        || sym.info.dealias.typeSymbol.match        // more ubiquity
           case dd.DummyImplicitClass | dd.SubTypeClass | dd.SameTypeClass => true
           case tps =>
             tps.isMarkerTrait // no members to use; was only if sym.name.is(ContextBoundParamName)
             ||                // but consider NotGiven
             tps.hasAnnotation(dd.LanguageFeatureMetaAnnot)
        || sym.info.isSingleton // DSL friendly
        || sym.info.dealias.isInstanceOf[RefinedType] // can't be expressed as a context bound
      if ctx.settings.WunusedHas.implicits
        && !infos.skip(m)
        && !m.isEffectivelyOverride
        && !allowed
      then
        if m.isPrimaryConstructor then
          val alias = m.owner.info.member(sym.name)
          if alias.exists then
            val aliasSym = alias.symbol
            val checking =
                 aliasSym.isAllOf(PrivateParamAccessor, butNot = CaseAccessor)
              || aliasSym.isAllOf(Protected | ParamAccessor, butNot = CaseAccessor) && m.owner.is(Given)
            if checking
              && !infos.refs(alias.symbol)
              && !usedByDefaultGetter(sym, m)
            then
              warnAt(pos)(UnusedSymbol.implicitParams(aliasSym))
        else if !usedByDefaultGetter(sym, m) then
          warnAt(pos)(UnusedSymbol.implicitParams(sym))

    def checkLocal(sym: Symbol, pos: SrcPos) =
      if ctx.settings.WunusedHas.locals
        && !sym.isOneOf(InlineProxy | Synthetic)
      then
        if sym.is(Mutable) && infos.asss(sym) then
          warnAt(pos)(UnusedSymbol.localVars)
        else
          warnAt(pos)(UnusedSymbol.localDefs)

    def checkPatvars() =
      // convert the one non-synthetic span so all are comparable; filter NoSpan below
      def uniformPos(sym: Symbol, pos: SrcPos): SrcPos =
        if pos.span.isSynthetic then pos else pos.sourcePos.withSpan(pos.span.toSynthetic)
      // patvars in for comprehensions share the pos of where the name was introduced
      val byPos = infos.pats.groupMap(uniformPos(_, _))((sym, pos) => sym)
      for (pos, syms) <- byPos if pos.span.exists && !syms.exists(_.hasAnnotation(defn.UnusedAnnot)) do
        if !syms.exists(infos.refs(_)) then
          if !syms.exists(v => !v.isLocal && !v.is(Private) || infos.nowarn(v)) then
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
      import scala.jdk.CollectionConverters.given
      type ImpSel = (Import, ImportSelector)
      def isUsed(sel: ImportSelector): Boolean = infos.sels.containsKey(sel)
      def warnImport(warnable: ImpSel, actions: List[CodeAction] = Nil): Unit =
        val (imp, sel) = warnable
        val msg = UnusedSymbol.imports(actions)
        // example collection.mutable.{Map as MutMap}
        val origin = cpy.Import(imp)(imp.expr, List(sel)).show(using ctx.withoutColors).stripPrefix("import ")
        warnAt(sel.srcPos)(msg, origin)

      if !actionable then
        for imp <- infos.imps.keySet.nn.asScala; sel <- imp.selectors if !isUsed(sel) do
          warnImport(imp -> sel)
      else
        // If the rest of the line is blank, include it in the final edit position. (Delete trailing whitespace.)
        // If for deletion, and the prefix of the line is also blank, then include that, too. (Del blank line.)
        // If deleting a blank line and surrounded by blank lines, remove an adjoining blank line.
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
          val p2 =
            if deleteLine then
              var newStart = prev + 1
              if srcPos.line > 1 then
                val source = srcPos.sourcePos.source
                import source.{lineToOffset, lineToOffsetOpt, offsetToLine}
                val startLine = offsetToLine(start)
                val endLine = offsetToLine(end)
                val preceding = lineToOffset(startLine - 1)
                lineToOffsetOpt(endLine + 2) match
                case Some(succeeding) if lineToOffset(startLine) - preceding == 1 && succeeding - end == 2 =>
                  newStart = preceding
                case _ =>
              p1.withStart(newStart)
            else p1
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
              imp.selectors.exists(!isUsed(_)) // check if any selector in statement was unused
          < nextImport then
            // if no usable selectors in the import statement, delete it entirely.
            // if there is exactly one usable selector, then replace with just that selector (i.e., format it).
            // else for each clause, delete it or format one selector or delete unused selectors.
            // To delete a comma separated item, delete start-to-start, but for last item delete a preceding comma.
            // Reminder that first clause span includes the keyword, so delete point-to-start instead.
            val existing = sortedImps.slice(index, nextImport)
            val (keeping, deleting) = existing.iterator.flatMap(imp => imp.selectors.map(imp -> _)).toList
                                      .partition((imp, sel) => isUsed(sel))
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
                case ref: TypeParamRef => termName(ref.binder.paramNames(ref.paramNum).toString.toLowerCase)
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
  def relax(tree: Tree, tpe: Type)(using Context): Unit =
    def refinements(tpe: Type, names: List[Name]): List[Name] =
      tpe match
      case RefinedType(parent, refinedName, refinedInfo) => refinedName :: refinements(parent, names)
      case _ => names
    val refinedNames = refinements(tpe, Nil)
    if !refinedNames.isEmpty then
      val names = refinedNames.toSet
      val relaxer = new TreeTraverser:
        def traverse(tree: Tree)(using Context) =
          tree match
          case tree: NamedDefTree if names(tree.name) => tree.withAttachment(NoWarn, ())
          case _ =>
          traverseChildren(tree)
      relaxer.traverse(tree)

  def ignoreArgsOfSelfConstruction(tree: Apply, ctor: Symbol)(using Context): Unit =
    val pars = ctor.denot.paramSymss.flatten.iterator.filter(_.isTerm)
    val args = allTermArguments(tree)
    for (par, arg) <- pars.zip(args) do
      if arg.symbol.is(ParamAccessor) && arg.symbol.name == par.name && arg.symbol.owner == ctor.owner then
        arg.putAttachment(Ignore, ())

  extension (nm: Name)
    inline def exists(p: Name => Boolean): Boolean = nm.ne(nme.NO_NAME) && p(nm)
    inline def isWildcard: Boolean = nm == nme.WILDCARD || nm.is(WildcardParamName)

  extension (tp: Type)(using Context)
    def importPrefix: Type = tp match
      case tp: NamedType => tp.prefix
      case tp: ClassInfo => tp.prefix
      case tp: TypeProxy => tp.superType.normalizedPrefix
      case _ => NoType
    def underlyingPrefix: Type = tp match
      case tp: NamedType => tp.prefix
      case tp: ClassInfo => tp.prefix
      case tp: TypeProxy => tp.underlying.underlyingPrefix
      case _ => NoType
    def skipPackageObject: Type =
      if tp.typeSymbol.isPackageObject then tp.underlyingPrefix else tp
    def underlying: Type = tp match
      case tp: TypeProxy => tp.underlying
      case _ => tp

  private val serializationNames: Set[TermName] =
    Set("readResolve", "readObject", "readObjectNoData", "writeObject", "writeReplace").map(termName(_))

  extension (sym: Symbol)(using Context)
    def isSerializationSupport: Boolean =
      sym.is(Method) && serializationNames(sym.name.toTermName) && sym.owner.isClass
        && sym.owner.derivesFrom(defn.JavaSerializableClass)
    def isMarkerTrait: Boolean =
      sym.info.hiBound.resultType.allMembers.forall: d =>
        val m = d.symbol
        !m.isTerm || m.isSelfSym || m.is(Method) && (m.owner == defn.AnyClass || m.owner == defn.ObjectClass)
    def isEffectivelyPrivate: Boolean =
      sym.is(Private, butNot = ParamAccessor)
      || sym.owner.isAnonymousClass && !sym.isEffectivelyOverride
    def isEffectivelyOverride: Boolean =
      sym.is(Override)
      ||
      sym.canMatchInheritedSymbols && { // inline allOverriddenSymbols using owner.info or thisType
        val owner = sym.owner.asClass
        val base = if owner.classInfo.selfInfo != NoType then owner.thisType else owner.info
        base.baseClasses.drop(1).iterator.exists(sym.overriddenSymbol(_).exists)
      }
    // pick the symbol the user wrote for purposes of tracking
    inline def userSymbol: Symbol=
      if sym.denot.is(ModuleClass) then sym.denot.companionModule else sym

  extension (sel: ImportSelector)
    def boundTpe: Type = sel.bound match
      case untpd.TypedSplice(tree) => tree.tpe
      case _ => NoType

  extension (imp: Import)(using Context)
    /** Is it the first import clause in a statement? `a.x` in `import a.x, b.{y, z}` */
    def isPrimaryClause: Boolean =
      imp.srcPos.span.pointDelta > 0 // primary clause starts at `import` keyword with point at clause proper

    /** Generated import of cases from enum companion. */
    def isGeneratedByEnum: Boolean =
      imp.symbol.exists && imp.symbol.owner.is(Enum, butNot = Case)

    /** No mechanism for detection yet. */
    def isCompiletimeTesting: Boolean =
      imp.expr.symbol == defn.CompiletimeTestingPackage//.moduleClass

  extension (pos: SrcPos)
    def isZeroExtentSynthetic: Boolean = pos.span.isSynthetic && pos.span.isZeroExtent
    def isSynthetic: Boolean = pos.span.isSynthetic && pos.span.exists
    def isUserCode(using Context): Boolean =
      val inlineds = enclosingInlineds // per current context
         inlineds.isEmpty
      || inlineds.exists(_.srcPos.sourcePos.contains(pos.sourcePos)) // include intermediate inlinings or quotes

  extension [A <: AnyRef](arr: Array[A])
    // returns `until` if not satisfied
    def indexSatisfying(from: Int, until: Int = arr.length)(p: A => Boolean): Int =
      var i = from
      while i < until && !p(arr(i)) do
        i += 1
      i
end CheckUnused
