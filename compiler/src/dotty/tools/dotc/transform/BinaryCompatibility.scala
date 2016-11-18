package dotty.tools.dotc
package transform

import core._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.DenotTransformers.{SymTransformer, IdentityDenotTransformer}
import Contexts.Context
import Symbols._
import Scopes._
import Flags._
import StdNames._
import SymDenotations._
import Types._
import collection.mutable
import TreeTransforms._
import Decorators._
import ast.Trees._
import TreeTransforms._
import java.io.File.separatorChar
import ValueClasses._

/** Checks that all non-private methods of a class are explicitly defined, not desugared and expressible in java
 */

class BinaryCompatibility extends MiniPhaseTransform with IdentityDenotTransformer { thisTransform =>
  import ast.tpd._

  override def phaseName: String = "binCompat"

  private def check(decl: DefTree /* DefDef | ValDef*/)(implicit ctx: Context) = {
    def checkUserDefined(x: Symbol): Boolean = {
      (x.isPrimaryConstructor || !x.symbol.flags.is(Synthetic)) && {
        val initialPeriod = x.initial.validFor.firstPhaseId
        val typerPeriod = ctx.typerPhase.period.firstPhaseId
        initialPeriod == typerPeriod &&
          ctx.atPhaseNotLaterThanTyper( implicit ctx =>
            x.symbol.exists && !x.symbol.flags.is(Synthetic)
          )
      }
    }

    def checkNotTransformed(x: Symbol): Boolean = {
      def compareDefTypes(erazed: Type, erazedCtx: Context, orig: Type, origCtx: Context): Boolean = {
        val ctx = 0 // shadow
        orig match {
          case _: ExprType| _: MethodType =>
            val mt = orig.asInstanceOf[MethodicType]
            val origParams = mt.paramTypess(origCtx).flatten
            val actualParams = erazed.paramTypess(erazedCtx).flatten

            origParams.hasSameLengthAs(actualParams) &&
              (actualParams zip origParams).forall(x =>
                compareDefTypes(x._1, erazedCtx, x._2, origCtx)
              ) && compareDefTypes(erazed.finalResultType(erazedCtx), erazedCtx, mt.finalResultType(origCtx), origCtx)
          case x: TypeRef =>
            erazed.paramTypess(erazedCtx).flatten.isEmpty &&
              x.classSymbol(origCtx) == erazed.finalResultType(erazedCtx).classSymbol(erazedCtx)
          case x: TypeAlias =>
            compareDefTypes(erazed: Type, erazedCtx: Context, x.alias, origCtx: Context)
          case x: AnnotatedType =>
            compareDefTypes(erazed: Type, erazedCtx: Context, x.underlying(origCtx), origCtx: Context)
          case x: RefinedType =>
            compareDefTypes(TypeErasure.erasure(x.refinedInfo)(erazedCtx), erazedCtx, x.refinedInfo, origCtx) &&
              compareDefTypes(erazed, erazedCtx, x.parent, origCtx)
          case _ => false
        }

      }
      ctx.atPhaseNotLaterThanTyper{ implicit ctx1 =>
        compareDefTypes(toDenot(x)(ctx).info(ctx), ctx, toDenot(x)(ctx1).info(ctx1), ctx1)
      }
    }


    if (!decl.symbol.is(Flags.Private))
      if (!checkUserDefined(decl.symbol))
        ctx.error(i"Public method ${decl.symbol.showFullName} was synthesized by compiler and is not guaranteed to be binary compatible.", decl.pos)
      else if (!checkNotTransformed(decl.symbol))
        ctx.error(i"${decl.symbol.showFullName} has a type transformed by compiler and is not guaranteed to be binary compatible.\n" +
          i"Original Type: ${ctx.atPhaseNotLaterThanTyper(implicit ctx => decl.symbol.info.show)}\n" +
          i"Transformed Type: ${decl.symbol.info}", decl.pos)
  }

  override def transformTypeDef(tree: TypeDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (tree.symbol.isClass && !tree.symbol.is(Flags.JavaDefined)
      && tree.symbol.hasAnnotation(ctx.definitions.ScalaBinaryCompatibleAnnot)) {
      val template = tree.rhs.asInstanceOf[Template]
      val decls = template.constr :: template.body

      if (tree.symbol.is(Flags.Trait))
        decls.foreach(x => {
          if (!x.symbol.flags.is(Deferred))
            ctx.error(i"${x.symbol.showFullName} has to be abstract to be guaranteed to be binary compatible in interface", x.pos)
          if (x.symbol.isTerm && !x.symbol.is(Flags.Method))
            ctx.error(i"Fields are not guaranteed to be binary compatible in traits", x.pos)
        })

      decls.foreach {
        case x: DefTree => check(x)
        case _ =>
      }

      //println(s"checking ${tree.symbol.showFullName}")
    }
    tree
  }
}
