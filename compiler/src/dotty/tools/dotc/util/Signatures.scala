package dotty.tools.dotc
package util

import ast.Trees._
import ast.tpd
import core.Constants.Constant
import core.Contexts._
import core.Denotations.SingleDenotation
import core.Flags
import core.NameOps.isUnapplyName
import core.Names._
import core.Types._
import util.Spans.Span
import core.Symbols.NoSymbol
import reporting._


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
    findEnclosingApply(path, span) match
      case tpd.EmptyTree => (0, 0, Nil)
      case UnApply(fun, _, patterns) => unapplyCallInfo(span, fun, patterns)
      case Apply(fun, params) => callInfo(span, params, fun)

  /**
   * Finds enclosing application from given `path` to `span`.
   *
   * @param path The path to the function application
   * @param span The position of the cursor
   * @return Tree which encloses closest application containing span.
   *         In case if cursor is pointing on closing parenthesis and
   *         next subsequent application exists, it returns the latter
   */
  private def findEnclosingApply(path: List[tpd.Tree], span: Span)(using Context): tpd.Tree =
    path.filterNot {
      case apply @ Apply(fun, _) => fun.span.contains(span) || isTuple(apply)
      case unapply @ UnApply(fun, _, _) => fun.span.contains(span) || isTuple(unapply)
      case _ => true
    } match {
      case Nil => tpd.EmptyTree
      case direct :: enclosing :: _ if direct.source(span.end -1) == ')' => enclosing
      case direct :: _ => direct
    }

  def callInfo(
    span: Span,
    params: List[tpd.Tree],
    fun: tpd.Tree
  )(using Context): (Int, Int, List[SingleDenotation]) =
    val (alternativeIndex, alternatives) = fun.tpe match {
      case err: ErrorType =>
        val (alternativeIndex, alternatives) = alternativesFromError(err, params) match {
          case (_, Nil) => (0, fun.denot.alternatives)
          case other => other
        }

        (alternativeIndex, alternatives)

      case _ =>
        val funSymbol = fun.symbol
        val alternatives = funSymbol.owner.info.member(funSymbol.name).alternatives
        val alternativeIndex = alternatives.map(_.symbol).indexOf(funSymbol) max 0
        (alternativeIndex, alternatives)
    }

    val curriedArguments = countParams(fun, alternatives(alternativeIndex))
    val paramIndex = params.indexWhere(_.span.contains(span)) match {
      case -1 => (params.length - 1 max 0) + curriedArguments
      case n => n + curriedArguments
    }

    (paramIndex, alternativeIndex, alternatives)

  private def unapplyCallInfo(
    span: Span,
    fun: tpd.Tree,
    patterns: List[tpd.Tree]
  )(using Context): (Int, Int, List[SingleDenotation]) =
    val patternPosition = patterns.indexWhere(_.span.contains(span))
    val activeParameter = extractParamTypess(fun.tpe.finalResultType.widen).headOption.map { params =>
      (patternPosition, patterns.length) match
        case (-1, 0) => 0 // there are no patterns yet so it must be first one
        case (-1, pos) => -1 // there are patterns, we must be outside range so we set no active parameter
        case _ => (params.size - 1) min patternPosition max 0 // handle unapplySeq to always highlight Seq[A] on elements
    }.getOrElse(-1)

    val appliedDenot = fun.symbol.asSingleDenotation.mapInfo(_ => fun.tpe) :: Nil
    (activeParameter, 0, appliedDenot)

  private def isTuple(tree: tpd.Tree)(using Context): Boolean =
    tree.symbol != NoSymbol && ctx.definitions.isTupleClass(tree.symbol.owner.companionClass)

  private def extractParamTypess(resultType: Type)(using Context): List[List[Type]] =
    resultType match {
      // Reference to a type which is not a type class
      case ref: TypeRef if !ref.symbol.isPrimitiveValueClass =>
        getExtractorMembers(ref)
      // Option or Some applied type. There is special syntax for multiple returned arguments:
      //   Option[TupleN] and Option[Seq],
      // We are not intrested in them, instead we extract proper type parameters from the Option type parameter.
      case AppliedType(TypeRef(_, cls), (appliedType @ AppliedType(tycon, args)) :: Nil)
          if (cls == ctx.definitions.OptionClass || cls == ctx.definitions.SomeClass) =>
        tycon match
          case TypeRef(_, cls) if cls == ctx.definitions.SeqClass => List(List(appliedType))
          case _ => List(args)
      // Applied type extractor. We must extract from applied type to retain type parameters
      case appliedType: AppliedType => getExtractorMembers(appliedType)
      // This is necessary to extract proper result type as unapply can return other methods eg. apply
      case MethodTpe(_, _, resultType) =>
        extractParamTypess(resultType.widenDealias)
      case _ =>
        Nil
    }

  // Returns extractors from given type. In case if there are no extractor methods it fallbacks to get method
  private def getExtractorMembers(resultType: Type)(using Context): List[List[Type]] =
    val productAccessors = resultType.memberDenots(
      underscoreMembersFilter,
      (name, buf) => buf += resultType.member(name).asSingleDenotation
    )
    val availableExtractors = if productAccessors.isEmpty then
      List(resultType.member(core.Names.termName("get")))
    else
      productAccessors
    List(availableExtractors.map(_.info.finalResultType.stripAnnots).toList)

  object underscoreMembersFilter extends NameFilter {
    def apply(pre: Type, name: Name)(using Context): Boolean = name.startsWith("_")
    def isStable = true
  }

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
        Signatures.Param(
          name.show,
          info.widenTermRefExpr.show,
          docComment.flatMap(_.paramDoc(name)),
          isImplicit = tp.isImplicitMethod)
      }

      params :: rest
    }

    def extractParamNamess(resultType: Type): List[List[Name]] =
      if resultType.typeSymbol.flags.is(Flags.CaseClass) && symbol.flags.is(Flags.Synthetic) then
        resultType.typeSymbol.primaryConstructor.paramInfo.paramNamess
      else
        Nil

    def toUnapplyParamss(method: Type)(using Context): List[Param] = {
      val resultType = method.finalResultType.widenDealias match
        case methodType: MethodType => methodType.resultType.widen
        case other => other

      val paramNames = extractParamNamess(resultType).flatten
      val paramTypes = extractParamTypess(resultType).flatten

      if paramNames.length == paramTypes.length then
        (paramNames zip paramTypes).map((name, info) => Param(name.show, info.show))
      else
        paramTypes.map(info => Param("", info.show))

    }

    denot.info.stripPoly match {
      case tpe if denot.name.isUnapplyName =>
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
   * The number of parameters before `tree` application. It is necessary to properly show
   * parameter number for erroneous applications before current one.
   *
   * This handles currying, so for an application such as `foo(1, 2)(3)`, the result of
   * `countParams` should be 3. It also takes into considerations unapplied arguments so for `foo(1)(3)`
   * we will still get 3, as first application `foo(1)` takes 2 parameters with currently only 1 applied.
   *
   * @param tree The tree to inspect.
   * @param denot Denotation of function we are trying to apply
   * @param alreadyCurried Number of subsequent Apply trees before current tree
   * @return The number of parameters that are passed.
   */
  private def countParams(tree: tpd.Tree, denot: SingleDenotation, alreadyCurried: Int = 0)(using Context): Int =
    tree match {
      case Apply(fun, params) =>
         countParams(fun, denot, alreadyCurried + 1) + denot.symbol.paramSymss(alreadyCurried).length
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
        case _                       => Nil

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
        case _ => 0
      }
    }
    val bestAlternative =
      if (alternativesScores.isEmpty) 0
      else alternativesScores.zipWithIndex.maxBy(_._1)._2

    (bestAlternative, alternatives)
  }
}


