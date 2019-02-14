package dotty.tools.dotc.util

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.Flags.Implicit
import dotty.tools.dotc.core.Names.TermName
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.dotc.core.Types.{ErrorType, MethodType, PolyType}
import dotty.tools.dotc.reporting.diagnostic.messages

import scala.collection.JavaConverters._

object Signatures {

  /**
   * Represent a method signature.
   *
   * @param name       The name of the method
   * @param tparams    The type parameters and their bounds
   * @param paramss    The parameter lists of this method
   * @param returnType The return type of this method, if this is not a constructor.
   * @param doc        The documentation for this method.
   */
  case class Signature(name: String, tparams: List[String], paramss: List[List[Param]], returnType: Option[String], doc: Option[String] = None) {
  }

  /**
   * Represent a method's parameter.
   *
   * @param name       The name of the parameter
   * @param tpe        The type of the parameter
   * @param doc        The documentation of this parameter
   * @param isImplicit Is this parameter implicit?
   */
  case class Param(name: String, tpe: String, doc: Option[String] = None, isImplicit: Boolean = false) {
    def show: String =
      s"$name: $tpe"
  }

  /**
   * Extract (current parameter index, function index, functions) out of a method call.
   *
   * @param path The path to the function application
   * @param span The position of the cursor
   * @return A triple containing the index of the parameter being edited, the index of the function
   *         being called, the list of overloads of this function).
   */
  def callInfo(path: List[tpd.Tree], span: Span)(implicit ctx: Context): (Int, Int, List[SingleDenotation]) = {
    path match {
      case Apply(fun, params) :: _ =>
        val alreadyAppliedCount = Signatures.countParams(fun)
        val paramIndex = params.indexWhere(_.span.contains(span)) match {
          case -1 => (params.length - 1 max 0) + alreadyAppliedCount
          case n => n + alreadyAppliedCount
        }

        val (alternativeIndex, alternatives) = fun.tpe match {
          case err: ErrorType =>
            val (alternativeIndex, alternatives) = alternativesFromError(err, params)
            (alternativeIndex, alternatives)

          case _ =>
            val funSymbol = fun.symbol
            val alternatives = funSymbol.owner.info.member(funSymbol.name).alternatives
            val alternativeIndex = alternatives.indexOf(funSymbol.denot) max 0
            (alternativeIndex, alternatives)
        }

        (paramIndex, alternativeIndex, alternatives)

      case _ =>
        (0, 0, Nil)
    }
  }

  def toSignature(denot: SingleDenotation)(implicit ctx: Context): Option[Signature] = {
    val symbol = denot.symbol
    val docComment = ParsedComment.docOf(symbol)
    val classTree = symbol.topLevelClass.asClass.rootTree

    def toParamss(tp: MethodType)(implicit ctx: Context): List[List[Param]] = {
      val rest = tp.resType match {
        case res: MethodType =>
          // Hide parameter lists consisting only of CanBuildFrom or DummyImplicit,
          // we can remove the CanBuildFrom special-case once we switch to the 2.13 standard library.
          if (res.resultType.isParameterless &&
              res.isImplicitMethod &&
              res.paramInfos.forall(info =>
                info.classSymbol.fullName.toString == "scala.collection.generic.CanBuildFrom" ||
                info.classSymbol.derivesFrom(ctx.definitions.DummyImplicitClass)))
            Nil
          else
            toParamss(res)
        case _ =>
          Nil
      }
      val params = tp.paramNames.zip(tp.paramInfos).map { case (name, info) =>
        Signatures.Param(name.show,
          info.widenTermRefExpr.show,
          docComment.flatMap(_.paramDoc(name)),
          isImplicit = tp.isImplicitMethod)
      }

      params :: rest
    }

    denot.info.stripPoly match {
      case tpe: MethodType =>
        val paramss = toParamss(tpe)
        val typeParams = denot.info match {
          case poly: PolyType =>
            poly.paramNames.zip(poly.paramInfos).map { case (x, y) => x.show + y.show }
          case _ =>
            Nil
        }

        val (name, returnType) =
          if (symbol.isConstructor) (symbol.owner.name.show, None)
          else (denot.name.show, Some(tpe.finalResultType.widenTermRefExpr.show))

        val signature =
          Signatures.Signature(name,
                               typeParams,
                               paramss,
                               returnType,
                               docComment.map(_.mainDoc))

        Some(signature)

      case other =>
        None
    }
  }

  /**
   * The number of parameters that are applied in `tree`.
   *
   * This handles currying, so for an application such as `foo(1, 2)(3)`, the result of
   * `countParams` should be 3.
   *
   * @param tree The tree to inspect.
   * @return The number of parameters that are passed.
   */
  private def countParams(tree: tpd.Tree): Int = {
    tree match {
      case Apply(fun, params) => countParams(fun) + params.length
      case _ => 0
    }
  }

  /**
   * Inspect `err` to determine, if it is an error related to application of an overloaded
   * function, what were the possible alternatives.
   *
   * If several alternatives are found, determines what is the best suited alternatives
   * given the parameters `params`: The alternative that has the most formal parameters
   * matching the given arguments is chosen.
   *
   * @param err    The error message to inspect.
   * @param params The parameters that were given at the call site.
   * @return A pair composed of the index of the best alternative (0 if no alternatives
   *         were found), and the list of alternatives.
   */
  private def alternativesFromError(err: ErrorType, params: List[tpd.Tree])(implicit ctx: Context): (Int, List[SingleDenotation]) = {
    val alternatives =
      err.msg match {
        case messages.AmbiguousOverload(_, alternatives, _) =>
          alternatives
        case messages.NoMatchingOverload(alternatives, _) =>
          alternatives
        case _ =>
          Nil
      }

      // If the user writes `foo(bar, <cursor>)`, the typer will insert a synthetic
      // `null` parameter: `foo(bar, null)`. This may influence what's the "best"
      // alternative, so we discard it.
      val userParams = params match {
        case xs :+ (nul @ Literal(Constant(null))) if nul.span.isZeroExtent => xs
        case _ => params
      }
      val userParamsTypes = userParams.map(_.tpe)

      // Assign a score to each alternative (how many parameters are correct so far), and
      // use that to determine what is the current active signature.
      val alternativesScores = alternatives.map { alt =>
        alt.info.stripPoly match {
          case tpe: MethodType =>
            userParamsTypes.zip(tpe.paramInfos).takeWhile{ case (t0, t1) => t0 <:< t1 }.size
          case _ =>
            0
        }
      }
      val bestAlternative =
        if (alternativesScores.isEmpty) 0
        else alternativesScores.zipWithIndex.maxBy(_._1)._2

      (bestAlternative, alternatives)
  }

}

