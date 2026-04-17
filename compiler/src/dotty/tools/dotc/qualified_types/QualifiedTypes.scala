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
import dotty.tools.dotc.core.Atoms
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.{ctx, Context}
import dotty.tools.dotc.core.Decorators.{em, i, toTermName}
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.{defn, Symbol}
import dotty.tools.dotc.core.Types.{
  AndType,
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

  /** Sticky attachment on an argument tree that records the `ENodeVar`
   *  (of kind `Skolem`) index allocated for its skolemized version inside
   *  qualifiers. Used by [[substParamInQualifiers]] to preserve identity
   *  across re-type-checks (typer / posttyper / Ycheck) — the same argTree
   *  always maps to the same skolem index, which is the key invariant that
   *  makes the EGraph recognize "the same unknown" across phases.
   */
  val QualifierSkolemIndex: Property.StickyKey[Int] = Property.StickyKey()

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
    val idx = argTree.getAttachment(QualifierSkolemIndex) match
      case Some(i) => i
      case None =>
        val i = ctx.base.qualifierSkolemIndexCounter.fresh()
        argTree.putAttachment(QualifierSkolemIndex, i)
        i
    val replacement = ENodeVar(ENodeVarKind.Skolem, idx)(argType)
    val replaceMap = new TypeMap:
      def apply(t: Type): Type = t match
        case QualifiedType(parent, qualifier) =>
          val parent1 = apply(parent)
          val qualifier1 =
            val substMap = new TypeMap:
              def apply(t2: Type): Type =
                if t2 eq pref then replacement
                else mapOver(t2)
            qualifier.mapTypes(substMap).asInstanceOf[ENode.Lambda]
          QualifiedType(parent1, qualifier1)
        case _ => mapOver(t)
    replaceMap(tp)

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
        if checkContainsSkolem(pt, tree.srcPos, mode) then
          tpd.evalOnce(tree): e =>
            If(
              e.isInstance(pt),
              e.asInstance(pt),
              Throw(New(defn.IllegalArgumentExceptionType, List()))
            )
        else
          tree.withType(ErrorType(em""))
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

  def checkContainsSkolem(
      tp: Type,
      pos: SrcPos,
      mode: QualifiedTypesMode = QualifiedTypesMode.Error
  )(using Context): Boolean =
    var res = true
    tp.foreachPart:
      case QualifiedType(_, qualifier) =>
        qualifier.foreachType: rootTp =>
          rootTp.foreachPart:
            case tp: SkolemType =>
              mode match
                case QualifiedTypesMode.Error | QualifiedTypesMode.RuntimeChecks =>
                  report.error(em"The qualified type $qualifier cannot be checked at runtime", pos)
                case QualifiedTypesMode.Warn =>
                  report.warning(em"The qualified type $qualifier cannot be checked at runtime", pos)
                case QualifiedTypesMode.Silent => ()
              res = false
            case tp: ENodeVar if tp.isFree =>
              mode match
                case QualifiedTypesMode.Error | QualifiedTypesMode.RuntimeChecks =>
                  report.error(em"The qualified type $qualifier cannot be checked at runtime", pos)
                case QualifiedTypesMode.Warn =>
                  report.warning(em"The qualified type $qualifier cannot be checked at runtime", pos)
                case QualifiedTypesMode.Silent => ()
              res = false
            case _ => ()
      case _ => ()
    res
