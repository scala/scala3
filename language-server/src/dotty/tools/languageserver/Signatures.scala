package dotty.tools.languageserver

import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.util.Positions.Position
import dotty.tools.dotc.core.Types.{ErrorType, MethodType}
import dotty.tools.dotc.reporting.diagnostic.messages

import org.eclipse.lsp4j.{ParameterInformation, SignatureInformation}

import scala.collection.JavaConverters._

object Signatures {

  def callInfo(path: List[Tree], pos: Position)(implicit ctx: Context): (Int, Int, List[SingleDenotation]) = {
    path match {
      case Apply(fun, params) :: _ =>
        val alreadyAppliedCount = Signatures.countParams(fun)
        val paramIndex = params.indexWhere(_.pos.contains(pos)) match {
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

  /**
   * The number of parameters that are applied in `tree`.
   *
   * This handles currying, so for an application such as `foo(1, 2)(3)`, the result of
   * `countParams` should be 3.
   *
   * @param tree The tree to inspect.
   * @return The number of parameters that are passed.
   */
  private def countParams(tree: Tree): Int = {
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
  private def alternativesFromError(err: ErrorType, params: List[Tree])(implicit ctx: Context): (Int, List[SingleDenotation]) = {
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
        case xs :+ (nul @ Literal(Constant(null))) if nul.pos.isZeroExtent => xs
        case _ => params
      }
      val userParamsTypes = userParams.map(_.tpe)

      // Assign a score to each alternative (how many parameters are correct so far), and
      // use that to determine what is the current active signature.
      val alternativesScores = alternatives.map { alt =>
        alt.info.stripPoly match {
          case tpe: MethodType =>
            userParamsTypes.zip(tpe.paramInfos).takeWhile(_ <:< _).size
          case _ =>
            0
        }
      }
      val bestAlternative =
        if (alternativesScores.isEmpty) 0
        else alternativesScores.zipWithIndex.maxBy(_._1)._2

      (bestAlternative, alternatives)
  }

  case class Signature(name: String, tparams: List[String], paramss: List[List[Param]], returnType: Option[String], doc: Option[String] = None) {
    def toSignatureInformation: SignatureInformation = {
      val paramInfoss = paramss.map(_.map(_.toParameterInformation))
      val paramLists = paramss.map { paramList =>
        val labels = paramList.map(_.show)
        val prefix = if (paramList.exists(_.isImplicit)) "implicit " else ""
        labels.mkString(prefix, ", ", "")
      }.mkString("(", ")(", ")")
      val tparamsLabel = if (tparams.isEmpty) "" else tparams.mkString("[", ", ", "]")
      val returnTypeLabel = returnType.map(t => s": $t").getOrElse("")
      val label = s"$name$tparamsLabel$paramLists$returnTypeLabel"
      val documentation = doc.map(DottyLanguageServer.hoverContent)
      val signature = new SignatureInformation(label)
      signature.setParameters(paramInfoss.flatten.asJava)
      documentation.foreach(signature.setDocumentation(_))
      signature
    }
  }

  case class Param(name: String, tpe: String, doc: Option[String] = None, isImplicit: Boolean = false) {

    def toParameterInformation: ParameterInformation = {
      val label = s"$name: $tpe"
      val documentation = doc.map(DottyLanguageServer.hoverContent)
      val info = new ParameterInformation(label)
      documentation.foreach(info.setDocumentation(_))
      info
    }

    def show: String =
      s"$name: $tpe"
  }
}
