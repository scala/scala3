package dotty.tools.dotc.qualified_types

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import dotty.tools.dotc.ast.TreeTypeMap
import dotty.tools.dotc.ast.tpd.{Apply, Block, Tree, TreeOps, TypeApply, TypeTree}
import dotty.tools.dotc.config.Feature
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.DenotTransformers.IdentityDenotTransformer
import dotty.tools.dotc.core.SymUtils
import dotty.tools.dotc.core.Types.{Type, TypeMap}
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.typer.LiftUnstable

final class LiftArgs extends MiniPhase:
  override def phaseName: String = "qualified_types.LiftArgs"

  override def description: String =
    "Lift arguments in qualified types used for runtime checks. This is needed so that there values can actually be used at runtime, without having to re-evaluate them."

  override def isRunnable(using Context) = super.isRunnable && Feature.qualifiedTypesEnabled

  override def transformApply(tree: Apply)(using Context): Tree =
    val argSkolemIds = tree.args.map(_.getAttachment(QualifiedTypes.QualifierSkolemIndex))

    if argSkolemIds.forall(_.isEmpty) then
      return tree

    val usedSkolemIds = mutable.Set[Int]()
    tree.args.foreach: arg =>
      arg.foreachSubTree:
        case TypeApply(fn, List(tpt: TypeTree)) if fn.symbol.isTypeTest =>
          tpt.tpe.foreachPart:
            case QualifiedType(_, qualifier) =>
              qualifier.foreachType:
                case ENodeVar(ENodeVarKind.Skolem, id) if argSkolemIds.contains(Some(id)) =>
                  usedSkolemIds += id
                case _ => ()
            case _ => ()
        case _ => ()

    if usedSkolemIds.isEmpty then
      return tree

    val liftedDefs = ListBuffer[Tree]()
    val liftedArgs = LiftUnstable.liftArgs(liftedDefs, tree.fun.tpe, tree.args)

    object SubstENodeVarsMap extends TypeMap:
      override def apply(tp: Type): Type = tp match
        case QualifiedType(parent, qualifier) =>
          val newQualifier = qualifier.mapTypes:
            case ENodeVar(ENodeVarKind.Skolem, id) if argSkolemIds.contains(Some(id)) =>
              val liftedArgIndex = argSkolemIds.indexOf(Some(id))
              liftedArgs(liftedArgIndex).tpe
            case tpe => tpe
          QualifiedType(parent, newQualifier.asInstanceOf[ENode.Lambda])
        case tpe => tpe

    val block0 = TreeTypeMap(SubstENodeVarsMap).transform(Block(
      liftedDefs.toList,
      cpy.Apply(tree)(tree.fun, liftedArgs.toList)
    )).asInstanceOf[Block]

    Block(block0.stats, block0.expr.cast(tree.tpe))
