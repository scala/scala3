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
import dotty.tools.dotc.config.Printers
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
  SkolemType,
  TermRef,
  Type,
  TypeProxy
}
import dotty.tools.dotc.util.SrcPos
import dotty.tools.dotc.report
import dotty.tools.dotc.reporting.trace

object QualifiedTypes:
  /** Does the type `tp1` imply the qualifier `qualifier2`?
   *
   *  Used by [[dotty.tools.dotc.core.TypeComparer]] to compare qualified types.
   *
   *  Note: the logic here is similar to [[Type#derivesAnnotWith]] but
   *  additionally handle comparisons with [[SingletonType]]s.
   */
  def typeImplies(tp1: Type, qualifier2: ENode.Lambda, solver: QualifierSolver)(using Context): Boolean =
    def trySelfifyType() =
      val ENode.Lambda(List(paramTp), _, _) = qualifier2: @unchecked
      ENode.selfify(tpd.singleton(tp1)) match
        case Some(qualifier1) => solver.implies(qualifier1, qualifier2)
        case None => false
    trace(i"typeImplies $tp1  -->  ${qualifier2.body}", Printers.qualifiedTypes):
      tp1 match
        case QualifiedType(parent1, qualifier1) =>
          solver.implies(qualifier1, qualifier2)
        case tp1: TermRef =>
          def trySelfifyRef() =
            tp1.underlying match
              case QualifiedType(_, _) => false
              case _ => trySelfifyType()
          typeImplies(tp1.underlying, qualifier2, solver) || trySelfifyRef()
        case tp1: ConstantType =>
          trySelfifyType()
        case tp1: TypeProxy =>
          typeImplies(tp1.underlying, qualifier2, solver)
        case AndType(tp11, tp12) =>
          typeImplies(tp11, qualifier2, solver) || typeImplies(tp12, qualifier2, solver)
        case OrType(tp11, tp12) =>
          typeImplies(tp11, qualifier2, solver) && typeImplies(tp12, qualifier2, solver)
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
        if tree.tpe.hasAnnotation(defn.RuntimeCheckedAnnot) then
          if checkContainsSkolem(pt, tree.srcPos) then
            tpd.evalOnce(tree): e =>
              If(
                e.isInstance(pt),
                e.asInstance(pt),
                Throw(New(defn.IllegalArgumentExceptionType, List()))
              )
          else
            tree.withType(ErrorType(em""))
        else
          ENode.selfify(tree) match
            case Some(qualifier) =>
              val selfifiedTp = QualifiedType(tree.tpe, qualifier)
              if selfifiedTp <:< pt then tree.cast(selfifiedTp) else EmptyTree
            case None =>
              EmptyTree
      else
        EmptyTree

  def containsQualifier(tp: Type)(using Context): Boolean =
    tp match
      case QualifiedType(_, _) => true
      case tp: TypeProxy       => containsQualifier(tp.underlying)
      case AndType(tp1, tp2)   => containsQualifier(tp1) || containsQualifier(tp2)
      case OrType(tp1, tp2)    => containsQualifier(tp1) || containsQualifier(tp2)
      case _                   => false

  def checkContainsSkolem(tp: Type, pos: SrcPos)(using Context): Boolean =
    var res = true
    tp.foreachPart:
      case QualifiedType(_, qualifier) =>
        qualifier.foreachType: rootTp =>
          rootTp.foreachPart:
            case tp: SkolemType =>
              report.error(em"The qualified type $qualifier cannot be checked at runtime", pos)
              res = false
            case _ => ()
      case _ => ()
    res
