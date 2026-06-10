package dotty.tools
package dotc
package transform

import core.*
import Annotations.Annotation
import Symbols.*, Types.*, Contexts.*, Flags.*, Decorators.*, reporting.*
import util.{EqHashSet, SrcPos}
import config.{ScalaVersion, NoScalaVersion, Feature}
import MegaPhase.MiniPhase
import scala.util.{Failure, Success}
import ast.tpd

class CrossVersionChecks extends MiniPhase:
  import tpd.*
  import CrossVersionChecks.*

  override def phaseName: String = CrossVersionChecks.name

  override def description: String = CrossVersionChecks.description

  /** Symbols already proven to carry no deprecated/experimental/preview
   *  annotation relevant to reference checks (and no migration annotation when
   *  `-Xmigration` is set). Whether a symbol carries such an annotation is a
   *  pure function of the symbol — position and context only matter once an
   *  annotation is found — and no phase in this group adds version annotations,
   *  so references to memoized symbols can skip the annotation walks entirely.
   *  Only the negative (annotation-free) verdict is cached; any symbol that
   *  could emit a diagnostic always takes the original check path.
   */
  private val cleanRefs = new EqHashSet[Symbol](8192)

  override def initContext(ctx: FreshContext): Unit =
    cleanRefs.clear()

  // Note: if a symbol has both @deprecated and @migration annotations and both
  // warnings are enabled, only the first one checked here will be emitted.
  // I assume that's a consequence of some code trying to avoid noise by suppressing
  // warnings after the first, but I think it'd be better if we didn't have to
  // arbitrarily choose one as more important than the other.
  private def checkUndesiredProperties(sym: Symbol, pos: SrcPos)(using Context): Unit =
    if !cleanRefs.contains(sym) then
      var clean = !checkRef(sym, pos)

      val xMigrationValue = ctx.settings.Xmigration.value
      if xMigrationValue != NoScalaVersion then
        clean &= !checkMigration(sym, pos, xMigrationValue)

      if clean then cleanRefs += sym
  end checkUndesiredProperties

  private def checkExperimentalAnnots(sym: Symbol)(using Context): Unit =
    if sym.exists && !sym.isInExperimentalScope then
      for annot <- sym.annotations if annot.symbol.isExperimental do
        Feature.checkExperimentalDef(annot.symbol, annot.tree)

  private def checkDeprecatedAnnots(sym: Symbol)(using Context): Unit =
    if sym.exists then
      for annot <- sym.annotations if annot.symbol.isDeprecated do
        checkDeprecatedRef(annot.symbol, annot.tree.srcPos)

  /** If @migration is present (indicating that the symbol has changed semantics between versions),
   *  emit a warning.
   *  @return whether a migration annotation is present on `sym`.
   */
  private def checkMigration(sym: Symbol, pos: SrcPos, xMigrationValue: ScalaVersion)(using Context): Boolean =
    val migrationAnnot = sym.getAnnotation(defn.MigrationAnnot)
    for annot <- migrationAnnot do
      val migrationVersion = ScalaVersion.parse(annot.argumentConstant(1).get.stringValue)
      migrationVersion match
        case Success(symVersion) if xMigrationValue < symVersion =>
          val msg = annot.argumentConstant(0).get.stringValue
          report.warning(SymbolChangedSemanticsInVersion(sym, symVersion, msg), pos)
        case Failure(ex) =>
          report.warning(SymbolHasUnparsableVersionNumber(sym, ex.getMessage.nn), pos)
        case _ =>
    migrationAnnot.isDefined

  /** Check that a deprecated val or def does not override a
   *  concrete, non-deprecated method.  If it does, then
   *  deprecation is meaningless.
   */
  private def checkDeprecatedOvers(tree: Tree)(using Context): Unit = {
    val symbol = tree.symbol
    if (symbol.isDeprecated) {
      val concrOvers =
        symbol.allOverriddenSymbols.filter(sym =>
          !sym.isDeprecated && !sym.is(Deferred))
      if (!concrOvers.isEmpty)
        report.deprecationWarning(
          em"""$symbol overrides concrete, non-deprecated definition(s):
              |    ${concrOvers.map(_.name).mkString(", ")}""",
          tree.srcPos)
    }
  }

  /** ??? */
  def checkDeprecatedInheritance(parents: List[Tree])(using Context): Unit = {
    for parent <- parents
        psym = parent.tpe.classSymbol
        annot <- psym.getAnnotation(defn.DeprecatedInheritanceAnnot)
        if !skipDeprecation(psym)
    do
      val msg = annot.argumentConstantString(0).map(msg => s": $msg").getOrElse("")
      val since = annot.argumentConstantString(1).map(version => s" (since: $version)").getOrElse("")
      val composed = em"inheritance from $psym is deprecated$since$msg"
      report.deprecationWarning(composed, parent.srcPos, origin = psym.showFullName)
  }

  private def unrollError(pos: SrcPos)(using Context): Unit =
    report.error(IllegalUnrollPlacement(None), pos)

  private def checkUnrollAnnot(annotSym: Symbol, pos: SrcPos)(using Context): Unit =
    if annotSym == defn.UnrollAnnot then
      unrollError(pos)

  private def checkUnrollMemberDef(memberDef: MemberDef)(using Context): Unit =
    val sym = memberDef.symbol
    if
      sym.hasAnnotation(defn.UnrollAnnot)
      && !(sym.isTerm && sym.is(Param))
    then
      val normSym = if sym.is(ModuleVal) then sym.moduleClass else sym
      unrollError(normSym.srcPos)

  override def transformValDef(tree: ValDef)(using Context): ValDef =
    checkUnrollMemberDef(tree)
    checkDeprecatedOvers(tree)
    checkExperimentalAnnots(tree.symbol)
    checkDeprecatedAnnots(tree.symbol)
    tree

  override def transformDefDef(tree: DefDef)(using Context): DefDef =
    checkUnrollMemberDef(tree)
    checkDeprecatedOvers(tree)
    checkExperimentalAnnots(tree.symbol)
    checkDeprecatedAnnots(tree.symbol)
    tree

  override def transformTypeDef(tree: TypeDef)(using Context): TypeDef =
    // TODO do we need to check checkDeprecatedOvers(tree)?
    checkUnrollMemberDef(tree)
    checkExperimentalAnnots(tree.symbol)
    checkDeprecatedAnnots(tree.symbol)
    tree

  override def transformTemplate(tree: tpd.Template)(using Context): tpd.Tree =
    checkDeprecatedInheritance(tree.parents)
    tree

  override def transformIdent(tree: Ident)(using Context): Ident = {
    checkUndesiredProperties(tree.symbol, tree.srcPos)
    tree
  }

  override def transformSelect(tree: Select)(using Context): Select = {
    checkUndesiredProperties(tree.symbol, tree.srcPos)
    tree
  }

  override def transformNew(tree: New)(using Context): New = {
    checkUndesiredProperties(tree.tpe.typeSymbol, tree.srcPos)
    tree
  }

  /** Check a `TypeRef` or `TermRef` part symbol of a `TypeTree`, memoizing
   *  symbols proven annotation-free. Memoization must cover all checks a later
   *  reference to the same symbol could need: when the deprecation check is
   *  skipped (span not source-derived) the deprecation verdict is established
   *  by the pure, non-reporting predicate `refersToDeprecated`, and the (pure)
   *  migration-annotation absence is required when `-Xmigration` is set.
   */
  private def checkTypeTreePart(sym: Symbol, tree: TypeTree)(using Context): Unit =
    if !cleanRefs.contains(sym) then
      val deprecated =
        if tree.span.isSourceDerived then checkDeprecatedRef(sym, tree.srcPos)
        else refersToDeprecated(sym)
      val experimental = checkExperimentalRef(sym, tree.srcPos)
      val preview = checkPreviewFeatureRef(sym, tree.srcPos)
      if !(deprecated | experimental | preview)
         && (ctx.settings.Xmigration.value == NoScalaVersion
             || !sym.hasAnnotation(defn.MigrationAnnot))
      then
        cleanRefs += sym

  override def transformTypeTree(tree: TypeTree)(using Context): TypeTree = {
    val tpe = tree.tpe
    tpe.foreachPart {
      case TypeRef(_, sym: Symbol)  =>
        checkTypeTreePart(sym, tree)
      case TermRef(_, sym: Symbol)  =>
        checkTypeTreePart(sym, tree)
      case AnnotatedType(_, annot) =>
        checkUnrollAnnot(annot.symbol, tree.srcPos)
      case _ =>
    }
    tree
  }

  override def transformOther(tree: Tree)(using Context): Tree =
    val inPackage = ctx.owner.is(Package) || ctx.owner.isPackageObject
    if !(inPackage && tree.isInstanceOf[ImportOrExport] && Feature.isExperimentalEnabledByImport) then
      tree.foreachSubTree { // Find references in type trees and imports
        case tree: Ident => transformIdent(tree)
        case tree: Select => transformSelect(tree)
        case tree: TypeTree => transformTypeTree(tree)
        case _ =>
      }
    tree match
      case Annotated(_, annot) =>
        checkUnrollAnnot(annot.tpe.typeSymbol, tree.srcPos)
        tree
      case tree => tree

end CrossVersionChecks

object CrossVersionChecks:
  val name: String = "crossVersionChecks"
  val description: String = "check issues related to deprecated and experimental"

  /** Check that a reference to an experimental definition with symbol `sym` meets cross-version constraints
   *  for `@deprecated`, `@experimental` and `@preview`.
   *  @return whether `sym` carries any such annotation (whether or not a
   *          diagnostic was emitted at this particular reference).
   */
  def checkRef(sym: Symbol, pos: SrcPos)(using Context): Boolean =
    val deprecated = checkDeprecatedRef(sym, pos)
    val experimental = checkExperimentalRef(sym, pos)
    val preview = checkPreviewFeatureRef(sym, pos)
    deprecated | experimental | preview

  /** Check that a reference to an experimental definition with symbol `sym` is only
   *  used in an experimental scope.
   *  @return whether `sym` is experimental.
   */
  private[CrossVersionChecks] def checkExperimentalRef(sym: Symbol, pos: SrcPos)(using Context): Boolean =
    val experimental = sym.isExperimental
    if experimental && !ctx.owner.isInExperimentalScope then
      Feature.checkExperimentalDef(sym, pos)
    experimental

  /** Check that a reference to a preview definition with symbol `sym` is only
   *  used in a preview mode.
   *  @return whether `sym` is a preview definition.
   */
  private[CrossVersionChecks] def checkPreviewFeatureRef(sym: Symbol, pos: SrcPos)(using Context): Boolean =
    val preview = sym.isPreview
    if preview && !ctx.owner.isInPreviewScope then
      Feature.checkPreviewDef(sym, pos)
    preview

  /** Does `checkDeprecatedRef` consider `sym` to refer to a deprecated
   *  definition, directly or via the synthetic case-companion lane?
   *  A pure function of the symbol; emits nothing.
   */
  private[CrossVersionChecks] def refersToDeprecated(sym: Symbol)(using Context): Boolean =
    sym.getAnnotation(defn.DeprecatedAnnot).isDefined
    || sym.isAllOf(SyntheticMethod)
       && {
         val companion = sym.owner.companionClass
         companion.is(CaseClass) && companion.getAnnotation(defn.DeprecatedAnnot).isDefined
       }

  /** If @deprecated is present, and the point of reference is not enclosed
   *  in either a deprecated member or a scala bridge method, issue a warning.
   *
   *  Also check for deprecation of the companion class for synthetic methods in the companion module.
   *
   *  @return whether `sym` refers to a deprecated definition (whether or not a
   *          warning was emitted at this particular reference).
   */
  private[CrossVersionChecks] def checkDeprecatedRef(sym: Symbol, pos: SrcPos)(using Context): Boolean =
    def warn(annotee: Symbol, annot: Annotation) =
      val message = annot.argumentConstantString(0).filter(!_.isEmpty).map(": " + _).getOrElse("")
      val since = annot.argumentConstantString(1).filter(!_.isEmpty).map(" since " + _).getOrElse("")
      val composed = em"${annotee.showLocated} is deprecated${since}${message}"
      report.deprecationWarning(composed, pos, origin = annotee.showFullName)
    sym.getAnnotation(defn.DeprecatedAnnot) match
      case Some(annot) =>
        if !skipDeprecation(sym) then warn(sym, annot)
        true
      case _ =>
        sym.isAllOf(SyntheticMethod)
        && {
          val companion = sym.owner.companionClass
          companion.is(CaseClass)
          && companion.getAnnotation(defn.DeprecatedAnnot).match
            case Some(annot) =>
              if !skipDeprecation(sym) then warn(companion, annot)
              true
            case _ => false
        }

  /** Decide whether the deprecation of `sym` should be ignored in this context.
   *
   *  The warning is skipped if any symbol in the context owner chain is deprecated,
   *  that is, an enclosing scope is associated with a deprecated symbol.
   *
   *  Further exclusions are needed for enums and case classes,
   *  since they typically need to refer to deprecated members
   *  even if the enclosing enum or case class is not deprecated.
   *
   *  If and only if `sym` is an enum case, the warning is skipped
   *  if an enclosing scope is either a module that declares `sym`,
   *  or the companion class of the module that declares `sym`.
   *
   *  For a deprecated case class or case class element,
   *  the warning is skipped for synthetic sites where the enclosing
   *  class (or its companion) is either the deprecated case class
   *  or the case class of the deprecated element.
   */
  def skipDeprecation(sym: Symbol)(using Context): Boolean =

    // is the owner an enum or its companion and also the owner of sym
    def isEnumOwner(owner: Symbol)(using Context) =
      // pre: sym is an enumcase
      if owner.isEnumClass then owner.companionClass eq sym.owner
      else if owner.is(ModuleClass) && owner.companionClass.isEnumClass then owner eq sym.owner
      else false

    def isDeprecatedOrEnum(owner: Symbol)(using Context) =
      // pre: sym is an enumcase
      owner.isDeprecated || isEnumOwner(owner)

    def siteIsEnclosedByDeprecatedElement =
      ctx.owner.ownersIterator.exists:
        if sym.isEnumCase then isDeprecatedOrEnum else _.isDeprecated

    def siteIsSyntheticCaseClassMember =
      val owner = ctx.owner
      def symIsCaseOrMember =
        val enclosing = owner.enclosingClass
        val companion = enclosing.companionClass
        // deprecated sym is either enclosing case class or a sibling member
        def checkSym(k: Symbol) = sym == k || sym.owner == k
        (enclosing.is(CaseClass) || companion.is(CaseClass)) && (checkSym(enclosing) || checkSym(companion))
      owner.is(Synthetic) && symIsCaseOrMember

    siteIsSyntheticCaseClassMember || siteIsEnclosedByDeprecatedElement
  end skipDeprecation
