package dotty.tools.dotc
package typer

import dotty.tools.dotc.ast.{ Trees, tpd }
import core.*
import Types.*, Contexts.*, Trees.*
import Decorators.*

object PreciseChecker:
  enum Mode:
    case SamePrecise, AllowMorePrecise, AllowLessPrecise
  def check(tparams: List[tpd.TypeDef], applied: Type, mode : Mode)(using Context): Unit =
    applied match
      case tpe@AppliedType(_, args) =>
        args.foreach(check(tparams, _, mode)) //recursively checking applied params
        val appliedParamPreciseList = tpe.tyconTypeParams.map(_.paramPrecise)
        val tdefParamPreciseMap = tparams.view.map(p => (p.name, p.symbol.paramPrecise)).toMap

        def label(precise: Boolean): String = if (precise) "precise" else "imprecise"
        args.view.zipWithIndex.foreach {
          case (a: TypeRef, i) if a.symbol.name.isTypeName =>
            val paramName = a.symbol.name.asTypeName
            val appliedParamPrecise = appliedParamPreciseList(i)
            tdefParamPreciseMap.get(paramName).foreach { tdefParamPrecise =>
              val preciseMismatch = mode match
                case Mode.SamePrecise => tdefParamPrecise != appliedParamPrecise
                case Mode.AllowMorePrecise => !tdefParamPrecise && appliedParamPrecise
                case Mode.AllowLessPrecise => tdefParamPrecise && !appliedParamPrecise
              if preciseMismatch then
                val pos = tparams.find(_.name == paramName).get.srcPos
                report.error(em"${label(tdefParamPrecise)} type parameter $paramName occurs in ${label(appliedParamPrecise)} position in $tpe", pos)
            }
          case _ =>
        }
      case _ =>

  def checkClass(tree: tpd.Template)(using Context): Unit =
    val tparams = tree.constr.leadingTypeParams
    tree.parents.view.map(_.tpe.dealias).foreach(check(tparams, _, Mode.AllowMorePrecise))

  def checkLambda(tree: tpd.LambdaTypeTree, isOpaque: Boolean)(using Context): Unit =
    tree.body.tpe.dealiasKeepOpaques match
      case at: AppliedType =>
        val mode = if (isOpaque) Mode.SamePrecise else Mode.AllowLessPrecise
        check(tree.tparams, at, mode)
      case tb: TypeBounds =>
        check(tree.tparams, tb.hi, Mode.AllowMorePrecise)
        check(tree.tparams, tb.lo, Mode.AllowLessPrecise)
      case _ =>
