package dotty.tools.dotc.transform

import scala.annotation.*

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
import dotty.tools.dotc.core.Types.{AnnotatedType, ClassInfo, ConstantType, NamedType, NoPrefix, NoType, TermRef, Type, TypeProxy, TypeTraverser}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.{SimpleName, DerivedName}
import dotty.tools.dotc.core.Names.{Name, TermName, termName}
import dotty.tools.dotc.core.NameOps.isReplWrapperName
import dotty.tools.dotc.core.NameKinds.{ContextFunctionParamName, WildcardParamName}
import dotty.tools.dotc.core.Symbols.{NoSymbol, Symbol, defn, isDeprecated}
import dotty.tools.dotc.report
import dotty.tools.dotc.reporting.{Message, UnusedSymbol as UnusedSymbolMessage}
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.typer.ImportInfo
import dotty.tools.dotc.util.{Property, SrcPos}
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.dotc.util.chaining.*

import scala.collection.mutable, mutable.{ArrayBuilder, ListBuffer, Stack}

import CheckUnused.*

/** A compiler phase that checks for unused imports or definitions.
 *
 *  Every construct that introduces a name must have at least one corresponding reference.
 *  The analysis is restricted to definitions of limited scope, i.e., private and local definitions.
 */
class CheckUnused private (phaseMode: PhaseMode, suffix: String) extends MiniPhase:

  override def phaseName: String = s"checkUnused$suffix"

  override def description: String = "check for unused elements"

  override def isEnabled(using Context): Boolean = ctx.settings.WunusedHas.any

  override def isRunnable(using Context): Boolean = super.isRunnable && ctx.settings.WunusedHas.any && !ctx.isJava

  override def prepareForUnit(tree: Tree)(using Context): Context =
    val infos = tree.getAttachment(refInfosKey).getOrElse(RefInfos())
    ctx.fresh.setProperty(refInfosKey, infos).tap(_ => tree.putAttachment(refInfosKey, infos))
  override def transformUnit(tree: Tree)(using Context): tree.type =
    if phaseMode == PhaseMode.Report then
      reportUnused()
    tree

  override def transformIdent(tree: Ident)(using Context): tree.type =
    if tree.symbol.exists then
      println(s"ID SYM ${tree.name} ${tree.symbol} in ${tree.typeOpt.importPrefix.skipPackageObject}")
      resolveUsage(tree.symbol, tree.name, tree.typeOpt.importPrefix.skipPackageObject)
    else if tree.hasType then
      println(s"ID TYP ${tree.name} ${tree.tpe.classSymbol} in ${tree.tpe.importPrefix.skipPackageObject}")
      resolveUsage(tree.tpe.classSymbol, tree.name, tree.tpe.importPrefix.skipPackageObject)
    tree

  override def transformSelect(tree: Select)(using Context): tree.type =
    val name = tree.removeAttachment(OriginalName).getOrElse(nme.NO_NAME)
    resolveUsage(tree.symbol, name, tree.qualifier.tpe)
    tree

  override def transformAssign(tree: Assign)(using Context): tree.type =
    val sym = tree.lhs.symbol
    if sym.exists then
      refInfos.asss.addOne(sym)
    tree

  override def transformTypeTree(tree: TypeTree)(using Context): tree.type =
    tree.tpe match
    case AnnotatedType(_, annot) => transformAllDeep(annot.tree)
    case tpt if !tree.isInferred && tpt.typeSymbol.exists =>
      resolveUsage(tpt.typeSymbol, tpt.typeSymbol.name, NoPrefix)
    case _ =>
    tree

  override def prepareForValDef(tree: ValDef)(using Context): Context =
    refInfos.register(tree)
    ctx
  override def transformValDef(tree: ValDef)(using Context): tree.type =
    traverseAnnotations(tree.symbol)
    tree

  override def prepareForDefDef(tree: DefDef)(using Context): Context =
    val rhs = tree.rhs
    val trivial =
          tree.symbol.is(Deferred)
       || rhs.symbol == ctx.definitions.Predef_undefined
       || rhs.tpe =:= ctx.definitions.NothingType
       || rhs.isInstanceOf[Literal]
       || rhs.tpe.match
          case ConstantType(_) => true
          case tp: TermRef => tp.underlying.classSymbol.is(Module) // Scala 2 SingleType
          case _ => false
    def nontrivial = tree.symbol.isAnonymousFunction
    if trivial && !nontrivial then refInfos.skip.addOne(tree.symbol)
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
    ctx.fresh.setProperty(resolvedKey, Resolved())
  override def transformTemplate(tree: Template)(using Context): Tree =
    tree

  override def prepareForPackageDef(tree: PackageDef)(using Context): Context =
    ctx.fresh.setProperty(resolvedKey, Resolved())
  override def transformPackageDef(tree: PackageDef)(using Context): Tree =
    tree

  override def prepareForStats(trees: List[Tree])(using Context): Context =
    ctx.fresh.setProperty(resolvedKey, Resolved())

  override def transformOther(tree: Tree)(using Context): tree.type =
    tree match
    case imp: Import =>
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
    case _: InferredTypeTree =>
    case _ if tree.isType =>
      //println(s"OTHER TYPE ${tree.getClass} ${tree.show}")
    case _ =>
      //println(s"OTHER ${tree.getClass} ${tree.show}")
    tree

  private def traverseAnnotations(sym: Symbol)(using Context): Unit =
    for annot <- sym.denot.annotations do
      transformAllDeep(annot.tree)

  /** Look up a reference in contexts to determine whether it was introduced by a definition or import.
   *
   *  The "usage" is recorded in the corresponding context. The reference is recorded here,
   *  since it's necessary to perform the lookup only once.
   *
   *  The first matching context must be correct; we are not rechecking name resolution.
   *  If there is no matching context, a root context must have been used for name resolution.
   */
  def resolveUsage(sym: Symbol, name: Name, prefix: Type = NoPrefix)(using Context): Unit =
    extension (info: ImportInfo) def matchingSelector: ImportSelector | Null =
      val qtpe = info.qualifier.tpe.nn
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
          def allSymbols = (
                qtpe.member(sel.name).alternatives
            ::: qtpe.member(sel.name.toTypeName).alternatives
          ).map(_.symbol).toSet
          // if there is an explicit name, it must match
          val matches = !name.exists(_.toTermName != sel.rename) &&
            (prefix.eq(NoPrefix) || qtpe =:= prefix) && allSymbols.contains(sym)
          if matches then sel else loop(sels)
        case nil => null
      loop(info.selectors)

    def checkMember(ctxsym: Symbol): Boolean =
      ctxsym.isClass && sym.owner.isClass
      && ctxsym.thisType.baseClasses.contains(sym.owner)
      && ctxsym.thisType.member(sym.name).alternatives.exists(_.containsSym(sym))

    def addCached(where: Context): Unit =
      where.property(resolvedKey) match
      case Some(res) =>
        val np = (name, prefix)
        res.seen.updateWith(sym):
          case svs @ Some(vs) => if vs.exists((n, p) => n == name && p =:= prefix) then svs else Some(np :: vs)
          case _ => Some(np :: Nil)
      case _ =>

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
    var importer: ImportSelector | Null = null // non-null for import context
    var precedence = Int.MaxValue // of current resolution
    val ctxs = ctx.outersIterator
    var done = false
    while !done && ctxs.hasNext do
      val cur = ctxs.next()
      if cur.owner eq sym then
        foundEnclosing = true
        done = true
      else if isLocal then
        if cur.owner eq sym.owner then done = true // only checking for enclosing
      else
        val cached =
          cur.property(resolvedKey) match
          case Some(res) => res.seen(sym).exists((n, p) => n == name && p =:= prefix)
          case _ => false
        if cached then
          println(s"CACHED $sym")
          candidate = cur
          done = true
        else if cur.isImportContext then
          val sel = cur.importInfo.nn.matchingSelector
          if sel != null then
            if sel.isWildcard then
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
            done = true
          else if precedence > 4 then
            precedence = 4
            candidate = cur
    end while
    //println(s"RESULT encl $foundEnclosing isLoc $isLocal cand $candidate imp $importer")
    if foundEnclosing then
      println(s"detected enclosing $sym")
    else if isLocal then
      refInfos.refs.addOne(sym)
    else if candidate != NoContext then
      refInfos.refs.addOne(sym)
      if candidate.isImportContext then
        if importer != null then
          refInfos.sels.addOne(importer)
      //addCached(candidate)
  end resolveUsage

end CheckUnused

object CheckUnused:

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

  class PostTyper extends CheckUnused(PhaseMode.Aggregate, "PostTyper")

  class PostInlining extends CheckUnused(PhaseMode.Report, "PostInlining")

  class RefInfos:
    val defs = mutable.Set.empty[(Symbol, SrcPos)]    // definitions
    val refs = mutable.Set.empty[Symbol]              // references
    val asss = mutable.Set.empty[Symbol]              // targets of assignment
    val skip = mutable.Set.empty[Symbol]              // methods to skip
    val imps = mutable.Set.empty[Import]              // imports
    val sels = mutable.Set.empty[ImportSelector]      // matched selectors
    def register(tree: Tree)(using Context): Unit =
      tree match
      case imp: Import =>
        if languageImport(imp.expr).isEmpty
          && !imp.isGeneratedByEnum
        then
          imps.addOne(imp)
      case tree: NamedDefTree =>
        if (tree.symbol ne NoSymbol) && !tree.name.isWildcard then
          defs.addOne((tree.symbol, tree.namePos))
      case _ =>
        if tree.symbol ne NoSymbol then
          defs.addOne((tree.symbol, tree.srcPos))

  // Symbols already resolved in the given Context (with name and prefix of lookup)
  class Resolved:
    val seen = mutable.Map.empty[Symbol, List[(Name, Type)]].withDefaultValue(Nil)

  def reportUnused()(using Context): Unit =
    /*
      case PatVars        => patVars
      */
    val warnings = ArrayBuilder.make[(UnusedSymbolMessage, SrcPos)]
    val infos = refInfos
    for (sym, pos) <- infos.defs.iterator if !sym.hasAnnotation(ctx.definitions.UnusedAnnot) do
      if infos.refs(sym) then
        if sym.isLocalToBlock then
          if ctx.settings.WunusedHas.locals && sym.is(Mutable) && !infos.asss(sym) then
            warnings.addOne((UnusedSymbolMessage.unsetLocals, pos))
        else if sym.isAllOf(Private | Mutable) && !infos.asss(sym) then
          warnings.addOne((UnusedSymbolMessage.unsetPrivates, pos))
      else if sym.is(Private, butNot = ParamAccessor) then
        if ctx.settings.WunusedHas.privates
          && !sym.isConstructor
          && sym.is(Private, butNot = SelfName | Synthetic | CaseAccessor)
        then
          warnings.addOne((UnusedSymbolMessage.privateMembers, pos))
      else if sym.is(Param, butNot = Given | Implicit) then
        val m = sym.owner
        if ctx.settings.WunusedHas.explicits
          && !infos.skip(m)
          && !sym.isAllOf(AccessorCreationFlags)
             // don't warn for class param with accessor alias
          //&& !(sym.owner.isPrimaryConstructor && sym.owner.owner.info.nonPrivateMember(sym.name).exists)
          && {
            // in constructors, class param is finally assigned to field.
            // For now, look for a like-named accessor that is
            //  - nonprivate or
            //  - is a case element which may be pattern matched
            //  - or is itself used
            val isAliasedClassParam = m.isPrimaryConstructor && {
              //val a = m.owner.info.nonPrivateMember(sym.name)
              val a = m.owner.info.member(sym.name)
              a.exists && {
                val asym = a.symbol
                val res = asym.is(ParamAccessor) && (!asym.is(Private) || asym.is(CaseAccessor) || infos.refs(asym))
                //println(s"CHK($res) $asym ${ asym.is(ParamAccessor)} ${ asym.is(Private)} ${ asym.is(CaseAccessor)} ${ infos.refs(asym)}")
                asym.is(ParamAccessor) && (!asym.is(Private) || asym.is(CaseAccessor) || infos.refs(asym))
              }
            }
            !isAliasedClassParam
          }
          && {
            val isExtendedReceiver = sym.owner.is(ExtensionMethod) && {
              val ps = m.paramSymss.dropWhile(_.exists(_.isTypeParam))
              ps match
              case (h :: Nil) :: Nil => h == sym
              case _ => false
            }
            !isExtendedReceiver
          }
          && !sym.name.isInstanceOf[DerivedName]
          /*
          && {
            sym.name match
            //case n: SimpleName => !n.contains('$')
            case n: DerivedName => false
            case _ => true
          }
          */
          && !ctx.platform.isMainMethod(m)
        then
          warnings.addOne((UnusedSymbolMessage.explicitParams, pos))
      else if sym.is(Param) then
        val m = sym.owner
        if ctx.settings.WunusedHas.implicits
          && !infos.skip(m)
        then
          warnings.addOne((UnusedSymbolMessage.implicitParams, pos))
      else if sym.isLocalToBlock then
        if ctx.settings.WunusedHas.locals then
          warnings.addOne((UnusedSymbolMessage.localDefs, pos))
    for imp <- infos.imps; sel <- imp.selectors if !sel.isImportExclusion && !infos.sels(sel) do
      warnings.addOne((UnusedSymbolMessage.imports, sel.srcPos))
    warnings.result().sortInPlaceBy(_._2.span.point).foreach(report.warning(_, _))

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
      imp.symbol.exists && imp.symbol.owner.is(Flags.Enum, butNot = Flags.Case)

end CheckUnused
