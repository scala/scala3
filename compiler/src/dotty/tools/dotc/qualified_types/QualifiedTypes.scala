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
import dotty.tools.dotc.core.Mode
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
  QualSkolemType,
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

  /** Side-channel from [[recordReceiverSkolem]] (called in `selectionType`) to
   *  [[wrapReceiverSkolem]] (called in `typedSelectWithAdapt`): the skolem index
   *  minted for an unstable selection prefix. Ephemeral — consumed within the same
   *  selection; stability across re-typing comes from `ENode.skolemFor` reading the
   *  index back off the marked receiver's type.
   */
  private val ReceiverSkolemIndex: Property.Key[Int] = Property.Key()

  /** Hook for `TypeAssigner.selectionType`. When selecting on an unstable receiver
   *  (`pre` is a `QualSkolemType`) and the selected member's type depends on the
   *  prefix through a qualifier, `findMember` has just minted an `ENodeVar.Skolem`
   *  for `pre`. Stash its index on the receiver tree `qual`, so
   *  [[wrapReceiverSkolem]] can mark the receiver with `@QualifierSkolemIndex`.
   *
   *  Recording the index makes the receiver skolem's identity stable across
   *  re-typing (`skolemFor` reads it back off the marked prefix), so the type
   *  round-trips through TASTy — exactly as arguments already do. It also lets
   *  [[ANF]] lift the receiver like any other dependent argument.
   *
   *  No-op when qualified types are off, the receiver is stable, or the member has
   *  no `this`-dependent qualifier (the `qualifierSkolemForSkolemType` cache is only
   *  populated by qualified-type skolemization, so `get` then returns `None`).
   */
  def recordReceiverSkolem(qual: Tree, pre: Type)(using Context): Unit =
    if Feature.qualifiedTypesEnabled then
      pre match
        case qsk: QualSkolemType =>
          ctx.base.qualifierSkolemForSkolemType.get(qsk).foreach: (_, idx) =>
            qual.putAttachment(ReceiverSkolemIndex, idx)
        case _ => ()

  /** Hook for `Typer.typedSelectWithAdapt`, counterpart to [[recordReceiverSkolem]]:
   *  if `qual` was stashed with a receiver-skolem index, wrap it with
   *  `@QualifierSkolemIndex` (same index) so [[ANF]] lifts the receiver into a `val`.
   *  Returns `qual` unchanged otherwise (qualified types off, or no stashed index).
   */
  def wrapReceiverSkolem(qual: Tree)(using Context): Tree =
    if !Feature.qualifiedTypesEnabled then qual
    else
      qual.removeAttachment(ReceiverSkolemIndex) match
        case Some(idx) => wrapWithSkolemIndex(qual, idx)
        case None => qual

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
    treeSkolemIndexOpt(tree, owner).getOrElse:
      // Allocate a fresh index without stamping. Consistency across
      // multiple calls only matters when the caller's qualifier
      // actually substitutes the result (a `QualifiedType` present in
      // its type), and in that case the arg was already wrapped by
      // `maybeWrapQualifiedArg` so `treeSkolemIndexOpt` returned `Some`
      // and we never reached this branch.
      (owner, ctx.base.freshSkolemIndex(owner))

  /** Like `treeSkolemIndex` but read-only: returns `None` if neither
   *  `tree.tpe` nor `tree.symbol` already carries a `@QualifierSkolemIndex`
   *  annotation. Used by callers that want to *transfer* an existing index
   *  (e.g. `EtaExpansion.lift`) without allocating a fresh one.
   */
  def treeSkolemIndexOpt(tree: Tree, owner: Symbol)(using Context): Option[(Symbol, Int)] =
    require(!tree.isEmpty, "Tree must be non-empty to have a skolem index attached")
    require(owner.exists, "Owner symbol must be valid to have a skolem index attached")
    trace(i"treeSkolemIndexOpt($tree, ${owner.show})", Printers.qualifiedTypes):
      readSkolemIndexAnnot(tree) match
        case Some(idx) => Some((owner, idx))
        case None =>
          val refSym = tpd.stripBlock(tree).symbol
          if refSym.exists then
            refSym.getAnnotation(defn.QualifierSkolemIndexAnnot) match
              case Some(annot) =>
                val tpd.Literal(Constant(i: Int)) :: Nil = annot.arguments: @unchecked
                return Some((owner, i))
              case None => ()
          None

  /** Extract the skolem index `n` from a tree whose type has been
   *  annotated `T @QualifierSkolemIndex(n)`, peeling through any leading
   *  `Typed`/`NamedArg` wrappers.
   */
  def readSkolemIndexAnnot(tree: Tree)(using Context): Option[Int] =
    def loop(t: Tree): Option[Int] = t match
      case tpd.Typed(expr, _) => readSkolemIndexAnnotType(t.tpe).orElse(loop(expr))
      case tpd.NamedArg(_, expr) => loop(expr)
      case _ => readSkolemIndexAnnotType(t.tpe)
    loop(tree)

  /** The skolem index carried by a `@QualifierSkolemIndex` annotation on the
   *  type `tp` (looking through other annotations), or `None`.
   */
  def readSkolemIndexAnnotType(tp: Type)(using Context): Option[Int] = tp match
    case AnnotatedType(parent, annot) if annot.symbol == defn.QualifierSkolemIndexAnnot =>
      annot.argument(0) match
        case Some(tpd.Literal(Constant(idx: Int))) => Some(idx)
        case _ => readSkolemIndexAnnotType(parent)
    case AnnotatedType(parent, _) => readSkolemIndexAnnotType(parent)
    case _ => None

  /** `tp` with any top-level `@QualifierSkolemIndex` annotations removed. The
   *  marker is metadata (a stable index carrier), not part of the type, so it
   *  must not leak into, e.g., an `ENodeVar.Skolem`'s underlying type — otherwise
   *  the type would differ depending on whether the marker is already present.
   */
  def dropSkolemIndexAnnot(tp: Type)(using Context): Type = tp match
    case AnnotatedType(parent, annot) if annot.symbol == defn.QualifierSkolemIndexAnnot =>
      dropSkolemIndexAnnot(parent)
    case at @ AnnotatedType(parent, annot) =>
      at.derivedAnnotatedType(dropSkolemIndexAnnot(parent), annot)
    case _ => tp

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
    else wrapWithSkolemIndex(arg, treeSkolemIndex(arg, skolemOwner)._2)

  /** Like [[wrapWithSkolemIndex]], but with a caller-supplied index `idx` (used
   *  for receivers, whose skolem index is minted by `ENode.skolemFor` rather than
   *  allocated from the tree). No-op if already wrapped or qualified types off.
   */
  def wrapWithSkolemIndex(arg: Tree, idx: Int)(using Context): Tree =
    if !Feature.qualifiedTypesEnabled || readSkolemIndexAnnot(arg).isDefined then arg
    else
      val annot = Annotation(defn.QualifierSkolemIndexAnnot, tpd.Literal(Constant(idx)), arg.span)
      val annotated = AnnotatedType(arg.tpe.widen, annot)
      tpd.Typed(arg, tpd.TypeTree(annotated, inferred = true))

  /** The `(owner, idx)` skolem identity stamped on `sym`, or `None` if it
   *  carries no `@QualifierSkolemIndex` annotation. Read-only — used by
   *  `termAssumptions` to bind an EtaExpansion-lifted val to its argument
   *  skolem without allocating a fresh index.
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
    // Without a qualifier anywhere in `tp`, both branches below are the
    // identity; skip the skolem-index allocation and the traversals.
    if !containsQualifier(tp) then return tp
    // A dependent call appearing *inside a qualifier predicate* (an annotation):
    // the predicate's `ENode` is built structurally from the tree, so this call's
    // own result qualifier is unused, and skolemizing its unstable argument would
    // only leak a skolem that nothing downstream can lift. Weaken the qualifier so
    // it no longer mentions `pref` instead. See [[ANF]].
    if ctx.mode.is(Mode.InAnnotation) then return dropParamInQualifiers(tp, pref)
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

  /** Weaken qualifiers inside `tp` so they no longer mention the parameter
   *  reference `pref`. Used in place of [[substParamInQualifiers]] when a
   *  dependent call appears *inside a qualifier predicate* (an annotation):
   *  there the predicate's `ENode` is built structurally from the tree, so the
   *  call's own result qualifier is unused, and skolemizing its unstable
   *  argument would only leak a skolem that nothing can later lift.
   */
  def dropParamInQualifiers(tp: Type, pref: ParamRef)(using Context): Type =
    if !Feature.qualifiedTypesEnabled then return tp
    avoidInQualifiers(tp):
      case t => t eq pref

  /** Weaken qualifiers inside `tp` so they no longer mention any symbol in
   *  `localSyms` — term references about to leave their scope. Boolean
   *  sub-expressions referring to such a symbol are approximated to `true`
   *  or `false` depending on polarity, exactly as [[avoidQualifierVars]]
   *  does for free `ENodeVar`s.
   */
  def avoidRefs(tp: Type, localSyms: List[Symbol])(using Context): Type =
    if !Feature.qualifiedTypesEnabled || localSyms.isEmpty then return tp
    val avoided = localSyms.toSet
    avoidInQualifiers(tp):
      case ref: TermRef => avoided.contains(ref.symbol)
      case _ => false

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
    avoidInQualifiers(tp):
      case v: ENodeVar => v.isFree
      case _ => false

  /** Weaken qualifiers inside `tp` by approximating every Boolean
   *  sub-expression that mentions a type leaf matching `avoid` to the
   *  polarity-appropriate constant (`true` in positive position, `false`
   *  in negative) so the rewritten qualifier is *weaker* than the original.
   *
   *  Variance-aware: the weaker approximation is a supertype (sound at
   *  variance > 0), the stronger one a subtype (sound at variance < 0). At
   *  variance 0 we hand a `Range` to the `ApproximatingTypeMap` framework,
   *  which propagates it outward and lets an enclosing variant constructor
   *  pick the appropriate bound. If `avoid` leaves cannot be eliminated
   *  (they appear outside any Boolean connective), the whole body collapses
   *  to a constant and the qualifier is dropped, falling back to the parent.
   */
  private def avoidInQualifiers(tp: Type)(avoid: Type => Boolean)(using Context): Type =
    val avoidMap = new ApproximatingTypeMap:
      def apply(t: Type): Type = t match
        case QualifiedType(parent, qualifier) =>
          val parent1 = apply(parent)
          if variance > 0 then
            makeQualifiedType(parent1, qualifier, avoidInBody(qualifier.body, positive = true, avoid))
          else if variance < 0 then
            makeQualifiedType(parent1, qualifier, avoidInBody(qualifier.body, positive = false, avoid))
          else
            val hi = makeQualifiedType(parent1, qualifier, avoidInBody(qualifier.body, positive = true, avoid))
            val lo = makeQualifiedType(parent1, qualifier, avoidInBody(qualifier.body, positive = false, avoid))
            if lo eq hi then lo else range(lo, hi)
        case _ => mapOver(t)
    avoidMap(tp)

  /** Recurse through Boolean connectives, replacing each sub-expression that
   *  contains a leaf matching `avoid` with the polarity-appropriate constant
   *  (`true` in positive position, `false` in negative). Identity-preserving
   *  when nothing is rewritten.
   */
  private def avoidInBody(node: ENode, positive: Boolean, avoid: Type => Boolean)(using Context): ENode =
    node match
      case ENode.OpApply(ENode.Op.And, args) =>
        val args1 = args.mapConserve(avoidInBody(_, positive, avoid))
        if args1 eq args then node else ENode.OpApply(ENode.Op.And, args1)
      case ENode.OpApply(ENode.Op.Or, args) =>
        val args1 = args.mapConserve(avoidInBody(_, positive, avoid))
        if args1 eq args then node else ENode.OpApply(ENode.Op.Or, args1)
      case ENode.OpApply(ENode.Op.Not, List(arg)) =>
        val arg1 = avoidInBody(arg, !positive, avoid)
        if arg1 eq arg then node else ENode.OpApply(ENode.Op.Not, List(arg1))
      case _ =>
        if containsMatching(node, avoid) then constantAtom(positive) else node

  private def constantAtom(value: Boolean)(using Context): ENode =
    ENode.Atom(ConstantType(Constant(value)))

  private def isTrueAtom(node: ENode)(using Context): Boolean = node match
    case ENode.Atom(ConstantType(Constant(true))) => true
    case _ => false

  private def isFalseAtom(node: ENode)(using Context): Boolean = node match
    case ENode.Atom(ConstantType(Constant(false))) => true
    case _ => false

  /** True iff any type leaf reachable from `node` satisfies `pred`. */
  private def containsMatching(node: ENode, pred: Type => Boolean)(using Context): Boolean =
    var found = false
    node.foreachType: tp =>
      tp.foreachPart(p => if pred(p) then found = true)
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

  /** Does `tp` contain a qualified type, possibly behind aliases? Memoized per
   *  type instance: the underlying scan (`existsPart` with `dealiasKeepAnnots`
   *  per part, forcing denotations) is expensive and runs on hot typer paths
   *  (branch adaptation, argument wrapping), where the same type instances
   *  recur. Provisional types are not cached: instantiation can change the
   *  answer.
   */
  def containsQualifier(tp: Type)(using Context): Boolean =
    val cache = ctx.base.containsQualifierCache
    val cached = cache.lookup(tp)
    if cached != null then cached.booleanValue
    else
      val res = tp.existsPart: p =>
        p.dealiasKeepAnnots match
          case QualifiedType(_, _) => true
          case _ => false
      if !tp.isProvisional then cache(tp) = res
      res
