package dotty.tools.dotc.qualified_types

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.{
  Apply,
  Block,
  EmptyTree,
  Ident,
  If,
  Lambda,
  Literal,
  New,
  Select,
  SeqLiteral,
  This,
  Throw,
  Tree,
  TypeApply,
  Typed,
  given
}
import dotty.tools.dotc.config.{Feature, Printers}
import dotty.tools.dotc.config.Feature.QualifiedTypesMode
import dotty.tools.dotc.core.Annotations.Annotation
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.{ctx, Context}
import dotty.tools.dotc.core.Decorators.{em, i}
import dotty.tools.dotc.core.Symbols.{defn, toDenot, NoSymbol, Symbol}
import dotty.tools.dotc.core.Types.{
  AndType,
  AnnotatedType,
  ApproximatingTypeMap,
  ConstantType,
  ErrorType,
  MethodType,
  OrType,
  ParamRef,
  SkolemType,
  TermRef,
  Type,
  TypeMap,
  TypeProxy
}
import dotty.tools.dotc.report
import dotty.tools.dotc.reporting.trace
import dotty.tools.dotc.util.{Property, SrcPos}

object QualifiedTypes:

  /** A key attached to casts inserted by `adaptByMode` for qualified type
   *  conversions that could not be verified statically.
   */
  val QualifiedTypeCast: Property.StickyKey[Unit] = Property.StickyKey()

  /** The symbol that scopes a skolem index: the closest enclosing method
   *  or class on the owner chain starting from `s`. Walks via
   *  `flagsUNSAFE` / `lastKnownDenotation` so it never forces a symbol's
   *  info — we may be called from a typer flow that is currently
   *  computing the very info we'd be forcing.
   *
   *  Falls back to `NoSymbol` only at the very root; in practice every
   *  caller is inside at least the root package class.
   */
  def skolemOwner(s: Symbol)(using Context): Symbol =
    if !s.exists then NoSymbol
    else if s.flagsUNSAFE.is(dotty.tools.dotc.core.Flags.Method) then s
    else if s.isClass then s
    else skolemOwner(s.lastKnownDenotation.maybeOwner)

  /** Skolem owner walked from `ctx.owner`. */
  def skolemOwner(using Context): Symbol = skolemOwner(ctx.owner)

  /** The `(owner, idx)` to use as the skolem identity for `tree`. Source
   *  of truth, in order:
   *
   *  1. A `@QualifierSkolemIndex(n)` annotation on `tree.tpe` (peeled
   *     through `Typed` / `NamedArg`). Stamped by `wrapWithSkolemIndex`
   *     and pickled into TASTy via the `Typed` ascription's
   *     `AnnotatedType`.
   *  2. The same annotation on `tree.symbol` (e.g. a reference to an
   *     EtaExpansion-lifted val). A stable `ref(lifted)` is not wrapped
   *     by `maybeWrapQualifiedArg` because it's already `isStable`, so
   *     its annotation lives on the symbol, not the tree.
   *  3. Otherwise: allocate a fresh index. This branch should only fire
   *     once per logical arg — callers must ensure the index gets stamped
   *     somewhere persistent (typically by `wrapWithSkolemIndex` rewriting
   *     the tree with the annotation, or `EtaExpansion.lift` stamping it
   *     on the lifted symbol).
   */
  def treeSkolemIndex(tree: Tree, owner: Symbol)(using Context): (Symbol, Int) =
    require(!tree.isEmpty, "Tree must be non-empty to have a skolem index attached")
    require(owner.exists, "Owner symbol must be valid to have a skolem index attached")
    trace(i"treeSkolemIndex($tree, ${owner.show})", Printers.qualifiedTypes):
      readSkolemIndexAnnot(tree) match
        case Some(idx) => (owner, idx)
        case None =>
          val refSym = tpd.stripBlock(tree).symbol
          if refSym.exists then
            refSym.getAnnotation(defn.QualifierSkolemIndexAnnot) match
              case Some(annot) =>
                val tpd.Literal(Constant(i: Int)) :: Nil = annot.arguments: @unchecked
                return (owner, i)
              case None => ()
          (owner, ctx.base.freshSkolemIndex(owner))

  /** Extract the skolem index `n` from a tree whose type has been
   *  annotated `T @QualifierSkolemIndex(n)`, peeling through any leading
   *  `Typed`/`NamedArg` wrappers.
   */
  def readSkolemIndexAnnot(tree: Tree)(using Context): Option[Int] =
    def fromType(tp: Type): Option[Int] = tp match
      case AnnotatedType(parent, annot) if annot.symbol == defn.QualifierSkolemIndexAnnot =>
        annot.argument(0) match
          case Some(tpd.Literal(Constant(idx: Int))) => Some(idx)
          case _ => fromType(parent)
      case AnnotatedType(parent, _) => fromType(parent)
      case _ => None
    def loop(t: Tree): Option[Int] = t match
      case tpd.Typed(expr, _) => fromType(t.tpe).orElse(loop(expr))
      case tpd.NamedArg(_, expr) => loop(expr)
      case _ => fromType(t.tpe)
    loop(tree)

  /** Wrap an argument tree as `Typed(arg, TypeTree(arg.tpe @QualifierSkolemIndex(n)))`
   *  so the per-arg skolem identity survives TASTy round-trips. No-op if
   *  the tree is already wrapped or qualified types are disabled.
   *
   *  Term annotations are carried via `Typed` (not `Annotated`, which is
   *  for type trees only). On re-typer the wrapper persists with the
   *  same `n`, so `treeSkolemIndex` returns the same `(owner, n)` pair.
   */
  def wrapWithSkolemIndex(arg: Tree)(using Context): Tree =
    if !Feature.qualifiedTypesEnabled then arg
    else
      readSkolemIndexAnnot(arg) match
        case Some(_) => arg
        case None =>
          val (_, idx) = treeSkolemIndex(arg, skolemOwner)
          val annot = Annotation(defn.QualifierSkolemIndexAnnot, tpd.Literal(Constant(idx)), arg.span)
          val annotated = AnnotatedType(arg.tpe.widen, annot)
          tpd.Typed(arg, tpd.TypeTree(annotated, inferred = true))

  /** Read or allocate the skolem index for `sym`. The annotation on the
   *  symbol is the source of truth; if absent we allocate a fresh index
   *  against the walked `skolemOwner(sym.owner)` and stamp the annotation
   *  so subsequent lookups (including post-TASTy) return the same `idx`.
   */
  def symbolSkolemIndex(sym: Symbol)(using Context): (Symbol, Int) =
    trace(i"symbolSkolemIndex(${sym.show})", Printers.qualifiedTypes):
      val owner = skolemOwner(sym.owner)
      val idx = sym.getAnnotation(defn.QualifierSkolemIndexAnnot) match
        case Some(annot) =>
          val tpd.Literal(Constant(i: Int)) :: Nil = annot.arguments: @unchecked
          i
        case None =>
          val fresh = ctx.base.freshSkolemIndex(owner)
          sym.addAnnotation(
            Annotation(defn.QualifierSkolemIndexAnnot, tpd.Literal(Constant(fresh)), sym.span)
          )
          fresh
      (owner, idx)

  /** Like `symbolSkolemIndex` but read-only: returns `None` if `sym`
   *  doesn't already have a `@QualifierSkolemIndex` annotation. Used by
   *  `termAssumptions` to decide whether to add an equality assumption
   *  binding a `TermRef` to its skolem — we don't want to allocate one
   *  here.
   */
  def symbolSkolemIndexOpt(sym: Symbol)(using Context): Option[(Symbol, Int)] =
    sym.getAnnotation(defn.QualifierSkolemIndexAnnot).map: annot =>
      val tpd.Literal(Constant(i: Int)) :: Nil = annot.arguments: @unchecked
      (skolemOwner(sym.owner), i)

  /** Inside any `@qualified` annotation occurring in `tp`, substitute
   *  the parameter reference `pref` with an `ENodeVar` (of kind `Skolem`)
   *  whose index is stable across invocations for the same `argTree` (via
   *  a sticky attachment). The `argType` is used as the underlying type of
   *  the new `ENodeVar`.
   *
   *  If `argTree` is null, `tp` is returned unchanged (the caller will handle
   *  the substitution outside qualifiers separately).
   *
   *  The rest of `tp` (outside qualifier annotations) is unchanged; callers
   *  typically follow up with a normal `substParam(pref, skolem)` for those
   *  positions.
   */
  def substParamInQualifiers(tp: Type, pref: ParamRef, argType: Type, argTree: tpd.Tree | Null)(using Context): Type =
    if argTree == null || !Feature.qualifiedTypesEnabled then return tp
    val (skolemOwnerSym, skolemIdx) = treeSkolemIndex(argTree, skolemOwner)
    val replacement = ENodeVar.Skolem(skolemOwnerSym, skolemIdx)(argType)
    val replaceMap = new TypeMap:
      def apply(t: Type): Type = t match
        case QualifiedType(parent, qualifier) =>
          val parent1 = apply(parent)
          val qualifier1 =
            val substMap = new TypeMap:
              def apply(t2: Type): Type =
                if t2 == pref then replacement
                else mapOver(t2)
            qualifier.mapTypes(substMap).asInstanceOf[ENode.Lambda]
          if (parent1 eq parent) && (qualifier1 eq qualifier) then t
          else QualifiedType(parent1, qualifier1)
        case _ => mapOver(t)
    replaceMap(tp)

  def avoidRefs(tp: Type, localSyms: List[Symbol])(using Context): Type =
    if !Feature.qualifiedTypesEnabled then return tp
    val avoidMap = new TypeMap:
      def apply(t: Type): Type = t match
        case QualifiedType(parent, qualifier) =>
          val parent1 = apply(parent)
          val qualifier1 =
            val innerMap = new TypeMap:
              def apply(t2: Type): Type = t2 match
                case ref: TermRef if localSyms.contains(ref.symbol) =>
                  val (skolemOwner, skolemIdx) = symbolSkolemIndex(ref.symbol)
                  ENodeVar.Skolem(skolemOwner, skolemIdx)(SkolemType(mapOver(ref.underlying)))
                case _ => mapOver(t2)
            qualifier.mapTypes(innerMap).asInstanceOf[ENode.Lambda]
          if (parent1 eq parent) && (qualifier1 eq qualifier) then t
          else QualifiedType(parent1, qualifier1)
        case _ => mapOver(t)
    avoidMap(tp)

  /** Builds the appropriate qualified type.
   *
   *  The returned type is simplified in the following cases:
   *  - If the body is `true`, the qualifier is vacuous and we return the
   *    parent type as-is (the qualifier imposes no constraints).
   *  - If the body is `false`, the qualifier is unsatisfiable and we return
   *    [[defn.NothingType]] (the qualifier cannot be satisfied, so the type
   *    is effectively empty).
   *
   *  @param parent the parent type of the qualified type
   *  @param qualifier the original qualifier lambda
   *  @param body the rewritten body of the qualifier
   */
  private def makeQualifiedType(parent: Type, qualifier: ENode.Lambda, body: ENode)(using Context): Type =
    if body eq qualifier.body then QualifiedType(parent, qualifier)
    else if isTrueAtom(body) then parent
    else if isFalseAtom(body) then defn.NothingType
    else QualifiedType(parent, qualifier.derived(qualifier.paramTps, qualifier.retTp, body))

  /** Weaken any qualifiers inside `tp` by eliminating free `ENodeVar`s
   *  (kinds `OpenedParam` and `Skolem`). Sub-expressions of Boolean type
   *  that contain such a var are replaced with `true` or `false` depending
   *  on polarity so that the rewritten qualifier is *weaker* than the
   *  original (i.e., implied by it).
   *
   *  If free vars cannot be eliminated from the qualifier's body by this
   *  rewriting (e.g., because they appear outside any Boolean connective),
   *  the qualifier is dropped entirely, falling back to the parent type.
   */
  def avoidQualifierVars(tp: Type)(using Context): Type =
    if !Feature.qualifiedTypesEnabled then return tp
    val avoidMap = new ApproximatingTypeMap:
      def apply(t: Type): Type = t match
        case QualifiedType(parent, qualifier) =>
          // Compute weaker (supertype, sound at variance > 0) and stronger
          // (subtype-shaped, sound at variance < 0) approximations.
          // Weakening replaces free-var leaves with `true` (drops constraints);
          // strengthening replaces them with `false` (forces unsatisfiability).
          // At variance 0 we hand a Range to the ApproximatingTypeMap framework,
          // which propagates it outward and lets an enclosing variant constructor
          // pick the appropriate bound.
          val parent1 = apply(parent)
          if variance > 0 then
            makeQualifiedType(parent1, qualifier, avoidVarsInBody(qualifier.body, positive = true))
          else if variance < 0 then
            makeQualifiedType(parent1, qualifier, avoidVarsInBody(qualifier.body, positive = false))
          else
            val hi = makeQualifiedType(parent1, qualifier, avoidVarsInBody(qualifier.body, positive = true))
            val lo = makeQualifiedType(parent1, qualifier, avoidVarsInBody(qualifier.body, positive = false))
            if lo eq hi then lo else range(lo, hi)
        case _ => mapOver(t)
    avoidMap(tp)

  /** Recurse through Boolean connectives, replacing sub-expressions that
   *  contain a free `ENodeVar` with the polarity-appropriate constant
   *  (`true` in positive position, `false` in negative position).
   */
  private def avoidVarsInBody(node: ENode, positive: Boolean)(using Context): ENode =
    node match
      case ENode.OpApply(ENode.Op.And, args) =>
        ENode.OpApply(ENode.Op.And, args.map(avoidVarsInBody(_, positive)))
      case ENode.OpApply(ENode.Op.Or, args) =>
        ENode.OpApply(ENode.Op.Or, args.map(avoidVarsInBody(_, positive)))
      case ENode.OpApply(ENode.Op.Not, List(arg)) =>
        ENode.OpApply(ENode.Op.Not, List(avoidVarsInBody(arg, !positive)))
      case _ =>
        if containsFreeVar(node) then constantAtom(positive)
        else node

  private def constantAtom(value: Boolean)(using Context): ENode =
    ENode.Atom(ConstantType(Constant(value)))

  private def isTrueAtom(node: ENode)(using Context): Boolean = node match
    case ENode.Atom(ConstantType(Constant(true))) => true
    case _ => false

  private def isFalseAtom(node: ENode)(using Context): Boolean = node match
    case ENode.Atom(ConstantType(Constant(false))) => true
    case _ => false

  private def containsFreeVar(node: ENode)(using Context): Boolean =
    var found = false
    node.foreachType: tp =>
      tp.foreachPart:
        case tp: ENodeVar if tp.isFree => found = true
        case _ =>
    found

  /** Does the type `tp1` imply the qualifier `qualifier2`?
   *
   *  Used by [[dotty.tools.dotc.core.TypeComparer]] to compare qualified types.
   *
   *  Note: the logic here is similar to [[Type#derivesAnnotWith]] but
   *  additionally handle comparisons with [[SingletonType]]s.
   */
  def typeImplies(tp1: Type, qualifier2: ENode.Lambda, solver: QualifierSolver)(using Context): Boolean =
    ctx.base.qualifiedTypesStats.record("QualifiedTypes.typeImplies"):
      typeImpliesRec(tp1, qualifier2, solver)

  def typeImpliesRec(tp1: Type, qualifier2: ENode.Lambda, solver: QualifierSolver)(using Context): Boolean =
    def trySelfifyType() =
      val ENode.Lambda(List(paramTp), _, _) = qualifier2: @unchecked
      ENode.selfify(tpd.singleton(tp1)) match
        case Some(qualifier1) => solver.implies(qualifier1, qualifier2)
        case None => false
    trace(i"typeImpliesRec $tp1  -->  ${qualifier2.body}", Printers.qualifiedTypes):
      tp1 match
        case QualifiedType(parent1, qualifier1) =>
          solver.implies(qualifier1, qualifier2)
        case tp1: TermRef =>
          def trySelfifyRef() =
            tp1.underlying match
              case QualifiedType(_, _) => false
              case _ => trySelfifyType()
          typeImpliesRec(tp1.underlying, qualifier2, solver) || trySelfifyRef()
        case tp1: ConstantType =>
          trySelfifyType()
        case tp1: TypeProxy =>
          typeImpliesRec(tp1.underlying, qualifier2, solver)
        case AndType(tp11, tp12) =>
          typeImpliesRec(tp11, qualifier2, solver) || typeImpliesRec(tp12, qualifier2, solver)
        case OrType(tp11, tp12) =>
          typeImpliesRec(tp11, qualifier2, solver) && typeImpliesRec(tp12, qualifier2, solver)
        case _ =>
          val trueQualifier: ENode.Lambda = ENode.Lambda(
            List(defn.AnyType),
            defn.BooleanType,
            ENode.Atom(ConstantType(Constant(true)))
          )
          solver.implies(trueQualifier, qualifier2)

  /** Try to adapt the tree to the given type `pt`
   *
   *  Returns [[EmptyTree]] if `pt` does not contain qualifiers or if the tree
   *  cannot be adapted, or the adapted tree otherwise.
   *
   *  Used by [[dotty.tools.dotc.core.Typer]].
   */
  def adapt(tree: Tree, pt: Type)(using Context): Tree =
    if containsQualifier(pt) then
      trace(i"adapt $tree to qualified type $pt", Printers.qualifiedTypes):
        val mode =
          if tree.tpe.hasAnnotation(defn.RuntimeCheckedAnnot) then QualifiedTypesMode.RuntimeChecks
          else Feature.qualifiedTypesMode
        ENode.selfify(tree) match
          case Some(qualifier) =>
            val selfifiedTp = QualifiedType(tree.tpe, qualifier)
            if selfifiedTp <:< pt then tree.cast(selfifiedTp)
            else adaptByMode(tree, pt, mode)
          case None =>
            adaptByMode(tree, pt, mode)
    else
      EmptyTree

  private def adaptByMode(tree: Tree, pt: Type, mode: QualifiedTypesMode)(using Context): Tree =
    mode match
      case QualifiedTypesMode.RuntimeChecks =>
        tpd.evalOnce(tree): e =>
          If(
            e.isInstance(pt),
            e.asInstance(pt),
            Throw(New(defn.IllegalArgumentExceptionType, List()))
          )
      case QualifiedTypesMode.Warn =>
        report.warning(em"Qualified type conversion from ${tree.tpe} to $pt cannot be verified statically", tree.srcPos)
        tree.cast(pt).withAttachment(QualifiedTypeCast, ())
      case QualifiedTypesMode.Silent =>
        tree.cast(pt).withAttachment(QualifiedTypeCast, ())
      case QualifiedTypesMode.Error =>
        EmptyTree

  def containsQualifier(tp: Type)(using Context): Boolean =
    tp match
      case QualifiedType(_, _) => true
      case tp: TypeProxy => containsQualifier(tp.underlying)
      case AndType(tp1, tp2) => containsQualifier(tp1) || containsQualifier(tp2)
      case OrType(tp1, tp2) => containsQualifier(tp1) || containsQualifier(tp2)
      case _ => false
