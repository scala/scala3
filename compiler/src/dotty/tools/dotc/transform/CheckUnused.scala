package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.desugar.{ForArtifact, PatternVar}
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.ast.untpd, untpd.ImportSelector
import dotty.tools.dotc.config.ScalaSettings
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Names.{Name, SimpleName, DerivedName, TermName, termName}
import dotty.tools.dotc.core.NameOps.{isAnonymousFunctionName, isReplWrapperName}
import dotty.tools.dotc.core.NameKinds.{ContextBoundParamName, ContextFunctionParamName, WildcardParamName}
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.{ClassSymbol, NoSymbol, Symbol, defn, isDeprecated, requiredClass, requiredModule}
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.report
import dotty.tools.dotc.reporting.UnusedSymbol
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.typer.ImportInfo
import dotty.tools.dotc.util.{Property, SrcPos}
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
    val infos = tree.getAttachment(refInfosKey).getOrElse {
      RefInfos().tap(tree.withAttachment(refInfosKey, _))
    }
    ctx.fresh.setProperty(refInfosKey, infos)
  override def transformUnit(tree: Tree)(using Context): tree.type =
    if phaseMode == PhaseMode.Report then
      reportUnused()
      tree.removeAttachment(refInfosKey)
    tree

  override def transformIdent(tree: Ident)(using Context): tree.type =
    if tree.symbol.exists then
      if !ignoreTree(tree) then
        resolveUsage(tree.symbol, tree.name, tree.typeOpt.importPrefix.skipPackageObject)
    else if tree.hasType then
      resolveUsage(tree.tpe.classSymbol, tree.name, tree.tpe.importPrefix.skipPackageObject)
    tree

  // import x.y; x may be rewritten x.y, also import x.z as y
  override def transformSelect(tree: Select)(using Context): tree.type =
    val name = tree.removeAttachment(OriginalName).getOrElse(nme.NO_NAME)
    if tree.qualifier.span.isSynthetic || name.exists(_ != tree.symbol.name) then
      if !ignoreTree(tree) then
        resolveUsage(tree.symbol, name, tree.qualifier.tpe)
    else
      refUsage(tree.symbol)
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

  override def transformInlined(tree: Inlined)(using Context): tree.type =
    if !tree.call.isEmpty then
      transformAllDeep(tree.call)
    tree

  override def prepareForBind(tree: Bind)(using Context): Context =
    refInfos.register(tree)
    ctx

  override def prepareForValDef(tree: ValDef)(using Context): Context =
    refInfos.register(tree)
    ctx
  override def transformValDef(tree: ValDef)(using Context): tree.type =
    traverseAnnotations(tree.symbol)
    if tree.name.startsWith("derived$") && tree.hasType then
      def loop(t: Tree): Unit = t match
        case Ident(name)  =>
          val target =
            val ts0 = t.tpe.typeSymbol
            if ts0.is(ModuleClass) then ts0.companionModule else ts0
          resolveUsage(target, name, t.tpe.underlyingPrefix.skipPackageObject)
        case Select(t, _) => loop(t)
        case _            =>
      tree.getAttachment(OriginalTypeClass).foreach(loop)
    tree

  override def prepareForDefDef(tree: DefDef)(using Context): Context =
    def isUnconsuming(rhs: Tree): Boolean =
          rhs.symbol == defn.Predef_undefined
       || rhs.tpe =:= defn.NothingType
       || rhs.isInstanceOf[Literal]
       || rhs.tpe.match
          case ConstantType(_) => true
          case tp: TermRef => tp.underlying.classSymbol.is(Module) // Scala 2 SingleType
          case _ => false
       //|| isPurePath(rhs) // a bit strong
       || rhs.match
          case Block((dd @ DefDef(anonfun, paramss, _, _)) :: Nil, Closure(Nil, Ident(nm), _)) =>
               isAnonymousFunctionName(anonfun)
            && anonfun == nm
            && paramss.match
               case (ValDef(contextual, _, _) :: Nil) :: Nil =>
                    contextual.is(ContextFunctionParamName)
                 && isUnconsuming(dd.rhs)
               case _ => false
          case This(_) => true
          case Ident(_) => rhs.symbol.is(ParamAccessor)
          case Typed(rhs, _) => isUnconsuming(rhs)
          case _ => false
    def trivial = tree.symbol.is(Deferred) || isUnconsuming(tree.rhs)
    def nontrivial = tree.symbol.isConstructor || tree.symbol.isAnonymousFunction
    if !nontrivial && trivial then refInfos.skip.addOne(tree.symbol)
    refInfos.register(tree)
    ctx
  override def transformDefDef(tree: DefDef)(using Context): tree.type =
    traverseAnnotations(tree.symbol)
    tree

  override def transformTypeDef(tree: TypeDef)(using Context): tree.type =
    traverseAnnotations(tree.symbol)
    if !tree.symbol.is(Param) then // type parameter to do?
      refInfos.register(tree)
    tree

  override def prepareForTemplate(tree: Template)(using Context): Context =
    if tree.symbol.name.isReplWrapperName then
      refInfos.isRepl = true
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
    case _: InferredTypeTree => // do nothing
    case _: Export => // nothing to do
    case _ if tree.isType =>
      println(s"OTHER TYPE ${tree.getClass} ${tree.show}")
    case _ =>
      println(s"OTHER ${tree.getClass} ${tree.show}")
    tree

  private def traverseAnnotations(sym: Symbol)(using Context): Unit =
    for annot <- sym.denot.annotations do
      transformAllDeep(annot.tree)

  /** Look up a reference in enclosing contexts to determine whether it was introduced by a definition or import.
   *  The binding of highest precedence must be correct.
   */
  def resolveUsage(sym: Symbol, name: Name, prefix: Type)(using Context): Unit =
    def matchingSelector(info: ImportInfo): ImportSelector | Null =
      val qtpe = info.site //info.qualifier.tpe.nn
      def loop(sels: List[ImportSelector]): ImportSelector | Null =
        sels match
        case sel :: sels if sel.isWildcard =>
          // the qualifier must have the target symbol as a member
          val matches = qtpe.member(sym.name).hasAltWith(_.symbol == sym)
            && {
              if sel.isGiven then // Further check that the symbol is a given or implicit and conforms to the bound
                sym.isOneOf(Given | Implicit)
                  && (sel.bound.isEmpty || sym.info.finalResultType <:< sel.boundTpe)
                  && (prefix.eq(NoPrefix) || qtpe =:= prefix)
              else
                !sym.is(Given) // Normal wildcard, check that the symbol is not a given (but can be implicit)
            }
          if matches then sel else loop(sels)
        case sel :: sels =>
          def hasAltMember(nm: Name) = qtpe.member(nm).hasAltWith(_.symbol == sym)
          // if there is an explicit name, it must match
          val matches = !name.exists(_.toTermName != sel.rename) &&
            (prefix.eq(NoPrefix) || qtpe =:= prefix) && (hasAltMember(sel.name) || hasAltMember(sel.name.toTypeName))
          if matches then sel else loop(sels)
        case nil => null
      loop(info.selectors)

    def checkMember(ctxsym: Symbol): Boolean =
      ctxsym.isClass && sym.owner.isClass
      && ctxsym.thisType.baseClasses.contains(sym.owner)
      && ctxsym.thisType.member(sym.name).hasAltWith(d => d.containsSym(sym) && !name.exists(_ != d.name))

    def addCached(where: Context): Unit =
      if where.moreProperties ne null then
        where.property(resolvedKey) match
        case Some(res) =>
          val np = (name, prefix)
          res.seen.updateWith(sym):
            case svs @ Some(vs) => if vs.exists((n, p) => n == name && p =:= prefix) then svs else Some(np :: vs)
            case _ => Some(np :: Nil)
        case _ =>

    if !sym.exists then return

    // Names are resolved by definitions and imports, which have four precedence levels:
    // 1. def from this compilation unit
    // 2. specific import
    // 3. wildcard import
    // 4. def from another compilation unit via enclosing package
    // We find the innermost, highest precedence. We have no nesting levels but assume correctness.
    // If the sym is an enclosing definition (the owner of a context), it does not count toward usages.
    val isLocal = sym.isLocalToBlock
    var foundEnclosing = false
    var candidate: Context = NoContext
    var cachePoint: Context = NoContext // last context with Resolved cache
    var importer: ImportSelector | Null = null // non-null for import context
    var precedence = Int.MaxValue // of current resolution; lower is higher
    var done = false
    var cached = false
    val ctxs = ctx.outersIterator
    while !done && ctxs.hasNext do
      val cur = ctxs.next()
      if cur.owner eq sym then
        foundEnclosing = true
        done = true
      else if isLocal then
        if cur.owner eq sym.owner then done = true // only checking for enclosing
      else
        cached =
          cur.property(resolvedKey) match
          case Some(res) =>
            if precedence > 4 then cachePoint = cur // conservative, cache must be nested below the result context
            res.saw(sym, name, prefix)
          case _ => false
        if cached then
          candidate = cur
          done = true
        else if cur.isImportContext then
          val sel = matchingSelector(cur.importInfo.nn)
          if sel != null then
            if cur.importInfo.nn.isRootImport then
              if precedence > 4 then
                precedence = 4
                candidate = cur
                importer = sel
              done = true
            else if sel.isWildcard then
              if precedence > 3 then
                precedence = 3
                candidate = cur
                importer = sel
            else
              if precedence > 2 then
                precedence = 2
                candidate = cur
                importer = sel
        else if checkMember(cur.owner) then
          if sym.srcPos.sourcePos.source == ctx.source then
            precedence = 1
            candidate = cur
            importer = null
            done = true
          else if precedence > 4 then
            precedence = 4
            candidate = cur
    end while
    if !foundEnclosing then
      refInfos.refs.addOne(sym)
      if phaseMode.eq(PhaseMode.Aggregate) && candidate != NoContext && candidate.isImportContext && importer != null
      then refInfos.sels.put(importer, ())
      if !cached then
        addCached(cachePoint)
      if cachePoint ne ctx then
        addCached(ctx)
  end resolveUsage
  // if sym is not an enclosing element, record the reference
  def refUsage(sym: Symbol)(using Context): Unit =
    if !ctx.outersIterator.exists(cur => cur.owner eq sym) then
      refInfos.refs.addOne(sym)
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

  /** Attachment holding the name of a type class as written by the user. */
  val OriginalTypeClass = Property.StickyKey[Tree]

  /** Suppress warning in a tree, such as a patvar absolved of unused warning by special naming convention. */
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
    def register(tree: Tree)(using Context): Unit =
      tree match
      case imp: Import =>
        if languageImport(imp.expr).isEmpty
          && !imp.isGeneratedByEnum
          //&& !imp.isTransparentInline
        then
          imps.put(imp, ())
      case tree: Bind =>
        if !tree.name.isInstanceOf[DerivedName] && !tree.name.is(WildcardParamName) && !tree.hasAttachment(NoWarn) then
          pats.addOne((tree.symbol, tree.namePos))
      case tree: ValDef if tree.hasAttachment(PatternVar) =>
        if !tree.name.isInstanceOf[DerivedName] then
          pats.addOne((tree.symbol, tree.namePos))
      case tree: NamedDefTree =>
        if (tree.symbol ne NoSymbol) && !tree.name.isWildcard then
          defs.addOne((tree.symbol, tree.namePos))
      case _ =>
        if tree.symbol ne NoSymbol then
          defs.addOne((tree.symbol, tree.srcPos))

    var isRepl = false // have we seen a REPL artifact? avoid import warning, for example
  end RefInfos

  // Symbols already resolved in the given Context (with name and prefix of lookup)
  class Resolved:
    val seen = mutable.Map.empty[Symbol, List[(Name, Type)]].withDefaultValue(Nil)
    def saw(sym: Symbol, name: Name, pre: Type)(using Context): Boolean =
      seen(sym).exists((n, p) => n == name && p =:= pre)

  def reportUnused()(using Context): Unit = warnings.foreach(report.warning(_, _))

  def warnings(using Context): Array[(UnusedSymbol, SrcPos)] =
    val warnings = ArrayBuilder.make[(UnusedSymbol, SrcPos)]
    val infos = refInfos
    for (sym, pos) <- infos.defs.iterator if !sym.hasAnnotation(defn.UnusedAnnot) do
      if infos.refs(sym) then
        if sym.isLocalToBlock then
          if ctx.settings.WunusedHas.locals && sym.is(Mutable) && !infos.asss(sym) then
            warnings.addOne((UnusedSymbol.unsetLocals, pos))
        else if sym.isAllOf(Private | Mutable) && !infos.asss(sym) then
          warnings.addOne((UnusedSymbol.unsetPrivates, pos))
      else if sym.is(Private, butNot = ParamAccessor) then
        if ctx.settings.WunusedHas.privates
          && !sym.isConstructor
          && sym.is(Private, butNot = SelfName | Synthetic | CaseAccessor)
          && !sym.isSerializationSupport
          && !(sym.is(Mutable) && sym.isSetter && sym.owner.is(Trait)) // tracks sym.underlyingSymbol sibling getter
        then
          warnings.addOne((UnusedSymbol.privateMembers, pos))
      else if sym.is(Param, butNot = Given | Implicit) then
        val m = sym.owner
        def forgiven(sym: Symbol) =
          val dd = defn
             sym.owner.isDeprecated
          || sym.owner.hasAnnotation(defn.UnusedAnnot) // param of unused method
          || sym.info.isSingleton
          || sym.owner.isConstructor && sym.owner.owner.thisType.baseClasses.contains(defn.AnnotationClass)
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
                    warnings.addOne((UnusedSymbol.explicitParams, pos))
                else
                  if ctx.settings.WunusedHas.privates then
                    warnings.addOne((UnusedSymbol.privateMembers, pos))
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
            warnings.addOne((UnusedSymbol.explicitParams, pos))
        end checkExplicit
        if !infos.skip(m)
          && !forgiven(sym)
        then
          checkExplicit()
      else if sym.is(Param) then // Given | Implicit
        val m = sym.owner
        def forgiven(sym: Symbol) =
          val dd = defn
             sym.name.is(ContextFunctionParamName)    // a ubiquitous parameter
          || sym.name.is(ContextBoundParamName)       // a ubiquitous parameter
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
          && !forgiven(sym)
        then
          if m.isPrimaryConstructor then
            val alias = m.owner.info.member(sym.name)
            if alias.exists then
              val aliasSym = alias.symbol
              if aliasSym.is(ParamAccessor) && !infos.refs(alias.symbol) then
                warnings.addOne((UnusedSymbol.implicitParams, pos))
          else
            warnings.addOne((UnusedSymbol.implicitParams, pos))
      else if sym.isLocalToBlock then
        if ctx.settings.WunusedHas.locals then
          warnings.addOne((UnusedSymbol.localDefs, pos))

    if ctx.settings.WunusedHas.patvars then
      // convert the one non-synthetic so all are comparable
      def uniformPos(sym: Symbol, pos: SrcPos): SrcPos =
        if pos.span.isSynthetic then pos else pos.sourcePos.withSpan(pos.span.toSynthetic)
      // patvars in for comprehensions have the pos of where the name was introduced
      val byPos = infos.pats.groupMap(uniformPos(_, _))((sym, pos) => sym)
      for (pos, syms) <- byPos if !syms.exists(_.hasAnnotation(defn.UnusedAnnot)) do
        if !syms.exists(infos.refs(_)) && !syms.exists(v => !v.isLocal && !v.is(Private)) then
          warnings.addOne((UnusedSymbol.patVars, pos))

    import scala.jdk.CollectionConverters.given
    val actionable = false && ctx.settings.rewrite.value.nonEmpty
    if (ctx.settings.WunusedHas.imports || ctx.settings.WunusedHas.strictNoImplicitWarn) && !infos.isRepl then
      for imp <- infos.imps.keySet.nn.asScala do
        if actionable then
          ???
        else
          for sel <- imp.selectors do
            if !sel.isImportExclusion && !infos.sels.containsKey(sel) && !imp.isLoose(sel) then
              warnings.addOne((UnusedSymbol.imports(actions = Nil), sel.srcPos))

    warnings.result().sorta(_._2.span.point)
  end warnings

  // Specific exclusions
  def ignoreTree(tree: Tree): Boolean =
    tree.hasAttachment(ForArtifact) || tree.hasAttachment(Ignore)

  // NoWarn Binds if the name matches a "canonical" name, e.g. case element name
  val nowarner = new TreeTraverser:
    def absolveVariableBindings(ok: List[Name], args: List[Tree]): Unit =
      ok.zip(args).foreach: (param, arg) =>
        arg match
        case Bind(`param`, _) => arg.withAttachment(NoWarn, ())
        case _ =>
    def traverse(tree: Tree)(using Context) = tree match
      case UnApply(fun, _, args) =>
        def untuple = defn.PairClass.companionModule.requiredMethod("unapply")
        val unapplied = tree.tpe.finalResultType.dealias.typeSymbol
        if unapplied.is(CaseClass) then
          absolveVariableBindings(unapplied.primaryConstructor.info.firstParamNames, args)
        else if fun.symbol == untuple then
          val ok = fun.symbol.info match
            case PolyType(tycon, MethodTpe(_, _, AppliedType(_, tprefs))) =>
              tprefs.collect:
                case ref: TypeParamRef => termName(ref.binder.paramNames(ref.paramNum).toString.toLowerCase.nn)
            case _ => Nil
          absolveVariableBindings(ok, args)
        else if fun.symbol == defn.TypeTest_unapply then
          () // just recurse into args
          /*
          fun match
          case Select(qual, nme.unapply) =>
            qual.tpe.underlying.finalResultType match
            case AppliedType(tycon, args) if tycon.typeSymbol == defn.TypeTestClass =>
              val target = args(1).dealias.typeSymbol
              if target.is(CaseClass) then
                absolveVariableBindings(target.primaryConstructor.info.firstParamNames, args)
            case _ =>
          case _ =>
          */
        else
          val Quotes_reflect: Symbol = defn.QuotesClass.requiredClass("reflectModule")
          if unapplied.exists && unapplied.owner == Quotes_reflect then
            // cheapy search for parameter names via java reflection of Trees
            // in lieu of drilling into requiredClass("scala.quoted.runtime.impl.QuotesImpl")
            // ...member("reflect")...member(unapplied.name.toTypeName)
            // with aliases into requiredModule("dotty.tools.dotc.ast.tpd")
            val implName = s"dotty.tools.dotc.ast.Trees$$${unapplied.name}"
            try
              import scala.language.unsafeNulls
              val clz = Class.forName(implName)
              val ok = clz.getConstructors.head.getParameters.map(p => termName(p.getName)).toList.init
              absolveVariableBindings(ok, args)
            catch case _: ClassNotFoundException => ()
        args.foreach(traverse)
      case tree => traverseChildren(tree)

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
    /** Generated import of cases from enum companion. */
    def isGeneratedByEnum(using Context): Boolean =
      imp.symbol.exists && imp.symbol.owner.is(Enum, butNot = Case)

    /** Checks if import selects a def that is transparent and inline.
    def isTransparentInline(using Context): Boolean =
      val qual = imp.expr
      imp.selectors.exists: sel =>
        val importedMembers = qual.tpe.member(sel.name).alternatives
        importedMembers.exists(_.symbol.isAllOf(Transparent | Inline))
     */

    /** Under -Wunused:strict-no-implicit-warn, avoid false positives
     *  if this selector is a wildcard that might import implicits or
     *  specifically does import an implicit.
     *  Similarly, import of CanEqual must not warn, as it is always witness.
     */
    def isLoose(sel: ImportSelector)(using Context): Boolean = ctx.settings.WunusedHas.strictNoImplicitWarn && (
         sel.isWildcard
      || imp.expr.tpe.member(sel.name.toTermName).hasAltWith(_.symbol.isOneOf(GivenOrImplicit))
      || imp.expr.tpe.member(sel.name.toTypeName).hasAltWith(_.symbol.isOneOf(GivenOrImplicit))
    ) || (
         sel.isWildcard && sel.isGiven
      && imp.expr.tpe.allMembers.exists(_.symbol.isCanEqual)
      || imp.expr.tpe.member(sel.name.toTermName).hasAltWith(_.symbol.isCanEqual)
    )

  // incredibly, there is no "sort in place" for array
  extension [A <: AnyRef](arr: Array[A])
    def sorta[B](f: A => B)(using ord: Ordering[B]): arr.type =
      import java.util.{Arrays, Comparator}
      val cmp = new Comparator[A | Null] {
        def compare(x: A, y: A): Int = ord.compare(f(x), f(y))
      }
      Arrays.sort(arr.asInstanceOf[Array[A | Null]], cmp)
      arr
end CheckUnused
