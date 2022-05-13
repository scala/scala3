package dotty.tools.dotc
package util

import ast.Trees._
import ast.tpd
import core.Constants.Constant
import core.Contexts._
import core.Denotations.SingleDenotation
import core.Flags
import core.Types._
import util.Spans.Span
import reporting._
import core.Names._


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
    def show: String = if name.nonEmpty then s"$name: $tpe" else tpe
  }

  /**
   * Extract (current parameter index, function index, functions) out of a method call.
   *
   * @param path The path to the function application
   * @param span The position of the cursor
   * @return A triple containing the index of the parameter being edited, the index of the function
   *         being called, the list of overloads of this function).
   */
  def callInfo(path: List[tpd.Tree], span: Span)(using Context): (Int, Int, List[SingleDenotation]) =
    val enclosingApply = path.find {
      case Apply(fun, _) => !fun.span.contains(span)
      case UnApply(fun, _, _) => !fun.span.contains(span)
      case _ => false
    }

    enclosingApply.map {
      case UnApply(fun, _, patterns) => unapplyCallInfo(span, fun, patterns)
      case Apply(fun, params) => callInfo(span, params, fun, Signatures.countParams(fun))
    }.getOrElse((0, 0, Nil))

  def callInfo( span: Span,
    params: List[tpd.Tree],
    fun: tpd.Tree,
    alreadyAppliedCount : Int
  )(using Context): (Int, Int, List[SingleDenotation]) =
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
        val alternativeIndex = alternatives.map(_.symbol).indexOf(funSymbol) max 0
        (alternativeIndex, alternatives)
    }

    (paramIndex, alternativeIndex, alternatives)

  private def unapplyCallInfo(span: Span,
    fun: tpd.Tree,
    patterns: List[tpd.Tree]
  )(using Context): (Int, Int, List[SingleDenotation]) =
    val paramIndex = patterns.indexWhere(_.span.contains(span)) max 0
    (paramIndex, 0, fun.symbol.asSingleDenotation.mapInfo(_ => fun.tpe) :: Nil)

  def toSignature(denot: SingleDenotation)(using Context): Option[Signature] = {
    val symbol = denot.symbol
    val docComment = ParsedComment.docOf(symbol)

    def toParamss(tp: MethodType)(using Context): List[List[Param]] = {
      val rest = tp.resType match {
        case res: MethodType =>
          // Hide parameter lists consisting only of DummyImplicit,
          if (res.resultType.isParameterless &&
              res.isImplicitMethod &&
              res.paramInfos.forall(info =>
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

    /**
     * This function is a hack which allows Signatures API to remain unchanged
     *
     * @return true if denot is "unapply", false otherwise
     */
    def isUnapplyDenotation: Boolean = denot.name equals core.Names.termName("unapply")

    def extractParamNamess(resultType: Type): List[List[Name]] =
      if resultType.resultType.widen.typeSymbol.flags.is(Flags.CaseClass) &&
          symbol.flags.is(Flags.Synthetic) then
        resultType.resultType.widen.typeSymbol.primaryConstructor.paramInfo.paramNamess
      else
        Nil

    def extractParamTypess(resultType: Type): List[List[Type]] =
      resultType match {
        case ref: TypeRef if !ref.symbol.isPrimitiveValueClass =>
          ref.symbol.primaryConstructor.paramInfo.paramInfoss
        case AppliedType(TypeRef(_, cls), AppliedType(_, args) :: Nil)
            if (cls == ctx.definitions.OptionClass || cls == ctx.definitions.SomeClass) =>
          List(args)
        case AppliedType(_, args) =>
          List(args)
        case MethodTpe(_, _, resultType) =>
          extractParamTypess(resultType)
        case _ =>
          Nil
      }

    def toUnapplyParamss(method: Type)(using Context): List[Param] = {
      val resultTpe = method.finalResultType.widenDealias
      val paramNames = extractParamNamess(resultTpe).flatten
      val paramTypes = extractParamTypess(resultTpe).flatten

      if paramNames.length == paramTypes.length then
        (paramNames zip paramTypes).map((name, info) => Param(name.show, info.show))
      else
        paramTypes.map(info => Param("", info.show))

    }

    denot.info.stripPoly match {
      case tpe if isUnapplyDenotation =>
        val params = toUnapplyParamss(tpe)
        if params.nonEmpty then
          Some(Signature("", Nil, List(params), None))
        else
          None

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
  private def countParams(tree: tpd.Tree): Int =
    tree match {
      case Apply(fun, params) => countParams(fun) + params.length
      case _ => 0
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
  private def alternativesFromError(err: ErrorType, params: List[tpd.Tree])(using Context): (Int, List[SingleDenotation]) = {
    val alternatives =
      err.msg match
        case msg: AmbiguousOverload  => msg.alternatives
        case msg: NoMatchingOverload => msg.alternatives
        case _                                => Nil

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


