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
import core.Symbols.NoSymbol
import interactive.Interactive
import util.Spans.Span
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
   * Extract (current parameter index, function index, functions) method call for given position.
   *
   * @param pos Position for which call should be returned
   * @return A triple containing the index of the parameter being edited, the index of functeon
   *         being called, the list of overloads of this function).
   */
  def signatureHelp(pos: SourcePosition)(using Context): (Int, Int, List[Signature]) = {
    val path = Interactive.pathTo(ctx.compilationUnit.tpdTree, pos.span)
    computeSignatureHelp(path, pos.span)(using Interactive.contextOfPath(path))
  }

  /**
   * Computes call info (current parameter index, function index, functions) for a method call.
   *
   * @param path The path to the function application
   * @param span The position of the cursor
   * @return A triple containing the index of the parameter being edited, the index of the function
   *         being called, the list of overloads of this function).
   */
  def computeSignatureHelp(path: List[tpd.Tree], span: Span)(using Context): (Int, Int, List[Signature]) =
    findEnclosingApply(path, span) match
      case tpd.EmptyTree => (0, 0, Nil)
      case Apply(fun, params) => applyCallInfo(span, params, fun)
      case UnApply(fun, _, patterns) => unapplyCallInfo(span, fun, patterns)

  /**
   * Finds enclosing application from given `path` for `span`.
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

  /**
   * Extracts call information for a function application.
   *
   * @param span The position of the cursor
   * @param params Current function parameters
   * @param fun Function tree which is being applied
   *
   * @return A triple containing the index of the parameter being edited, the index of the function
   *         being called, the list of overloads of this function).
   */
  private def applyCallInfo(
    span: Span,
    params: List[tpd.Tree],
    fun: tpd.Tree
  )(using Context): (Int, Int, List[Signature]) =
    val (alternativeIndex, alternatives) = fun.tpe match
      case err: ErrorType =>
        val (alternativeIndex, alternatives) = alternativesFromError(err, params) match
          case (_, Nil) => (0, fun.denot.alternatives)
          case other => other
        (alternativeIndex, alternatives)
      case _ =>
        val funSymbol = fun.symbol
        val alternatives = funSymbol.owner.info.member(funSymbol.name).alternatives
        val alternativeIndex = alternatives.map(_.symbol).indexOf(funSymbol) max 0
        (alternativeIndex, alternatives)

    val curriedArguments = countParams(fun, alternatives(alternativeIndex))
    val paramIndex = params.indexWhere(_.span.contains(span)) match {
      case -1 => (params.length - 1 max 0) + curriedArguments
      case n => n + curriedArguments
    }
    val alternativeSignatures = alternatives.flatMap(toApplySignature)
    (paramIndex, alternativeIndex, alternativeSignatures)

  /**
   * Extracts call informatioin for function in unapply context.
   *
   * @param span The position of the cursor
   * @param params Current function parameters
   * @param fun Unapply function tree
   *
   * @return A triple containing the index of the parameter being edited, the index of the function
   *         being called, the list of overloads of this function).
   */
  private def unapplyCallInfo(
    span: Span,
    fun: tpd.Tree,
    patterns: List[tpd.Tree]
  )(using Context): (Int, Int, List[Signature]) =
    val resultType = unapplyMethodResult(fun)
    val isUnapplySeq = fun.denot.name == core.Names.termName("unapplySeq")
    val denot = fun.denot

    def extractParamTypess(resultType: Type): List[List[Type]] =
      resultType match
        // Reference to a type which is not a type class
        case ref: TypeRef if !ref.symbol.isPrimitiveValueClass => mapOptionLessUnapply(ref, patterns.size, isUnapplySeq)
        // Option or Some applied type. There is special syntax for multiple returned arguments: Option[TupleN]
        // We are not intrested in it, instead we extract proper type parameters from the Option type parameter.
        case AppliedType(TypeRef(_, cls), (appliedType @ AppliedType(tycon, args)) :: Nil)
            if (cls == ctx.definitions.OptionClass || cls == ctx.definitions.SomeClass) =>
          tycon match
            case typeRef: TypeRef if ctx.definitions.isTupleClass(typeRef.symbol) => List(args)
            case _ => List(List(appliedType))
        // Applied type extractor. We must extract from applied type to retain type parameters
        case appliedType: AppliedType => mapOptionLessUnapply(appliedType, patterns.size, isUnapplySeq)
        // This is necessary to extract proper result type as unapply can return other methods eg. apply
        case MethodTpe(_, _, resultType) => extractParamTypess(resultType.widenDealias)
        case _ => Nil

    def extractParamNamess(resultType: Type): List[List[Name]] =
      if resultType.typeSymbol.flags.is(Flags.CaseClass) && denot.symbol.flags.is(Flags.Synthetic) then
        resultType.typeSymbol.primaryConstructor.paramInfo.paramNamess
      else
        Nil

    val paramTypes = extractParamTypess(resultType).flatten.map(stripAllAnnots)
    val paramNames = extractParamNamess(resultType).flatten

    val activeParameter = unapplyParameterIndex(patterns, span, paramTypes.length)
    val unapplySignature = toUnapplySignature(paramNames, paramTypes).toList

    (activeParameter, 0, unapplySignature)

  /**
   * Recursively strips annotations from given type
   *
   * @param tpe Type to strip annotations from
   * @return Type with stripped annotations
   */
  private def stripAllAnnots(tpe: Type)(using Context): Type = tpe match
    case AppliedType(t, args) => AppliedType(stripAllAnnots(t), args.map(stripAllAnnots))
    case other => other.stripAnnots

  /**
   * Get index of currently edited parameter in unapply context.
   *
   * @param patterns Currently applied patterns for unapply method
   * @param span The position of the cursor
   * @param maximumParams Number of parameters taken by unapply method
   * @return Index of currently edited parameter
   */
  private def unapplyParameterIndex(patterns: List[tpd.Tree], span: Span, maximumParams: Int)(using Context): Int =
    val patternPosition = patterns.indexWhere(_.span.contains(span))
    (patternPosition, patterns.length) match
      case (-1, 0) => 0 // there are no patterns yet so it must be first one
      case (-1, pos) => -1 // there are patterns, we must be outside range so we set no active parameter
      case _ => (maximumParams - 1) min patternPosition max 0 // handle unapplySeq to always highlight Seq[A] on elements

  private def isTuple(tree: tpd.Tree)(using Context): Boolean =
    tree.symbol != NoSymbol && ctx.definitions.isTupleClass(tree.symbol.owner.companionClass)

  /**
   * Get unapply method result type omiting unknown types and another method calls.
   *
   * @param fun Unapply tree
   * @return Proper unapply method type after extracting result from method types and omiting unknown types.
   */
  private def unapplyMethodResult(fun: tpd.Tree)(using Context): Type =
    val typeWithoutBinds = fun match
      case TypeApply(_, Bind(_, _) :: _) => fun.symbol.asSingleDenotation.info
      case other => other.tpe

    typeWithoutBinds.finalResultType.widenDealias match
      case methodType: MethodType => methodType.resultType.widen
      case other => other

  /**
   *  Maps type by checking if given match is single match, name-based match, sequence match or product sequence match.
   *  The precedence in higher priority - higher index order for fixed-arity extractors is:
   *    1. Single match
   *    2. Name based match
   *  For variadic extractors:
   *    1. Sequence match
   *    2. Product sequence match
   *
   *  @see [[https://docs.scala-lang.org/scala3/reference/changed-features/pattern-matching.html]]
   *
   *  @param resultType Final result type for unapply
   *  @param patternCount Currently applied patterns to unapply function
   *  @param isUnapplySeq true if unapply name equals "unapplySeq", false otherwise
   *  @return List of List of types dependent on option less extractor type.
   */
  private def mapOptionLessUnapply(
    resultType: Type,
    patternCount: Int,
    isUnapplySeq: Boolean
  )(using Context): List[List[Type]] =
    val productAccessors = resultType.memberDenots(
      underscoreMembersFilter,
      (name, buf) => buf += resultType.member(name).asSingleDenotation
    )

    val getMethod = resultType.member(core.Names.termName("get"))
    val dropMethod = resultType.member(core.Names.termName("drop"))

    val availableExtractors = (getMethod.exists && patternCount <= 1, isUnapplySeq && dropMethod.exists) match
      case (_, true) => List(dropMethod)
      case (true, _) => List(getMethod)
      case _  => productAccessors

    List(availableExtractors.map(_.info.finalResultType).toList)

  /**
   * Filter returning only members starting with underscore followed with number
   */
  private object underscoreMembersFilter extends NameFilter {
    def apply(pre: Type, name: Name)(using Context): Boolean =
      name.startsWith("_") && name.toString.drop(1).toIntOption.isDefined
    def isStable = true
  }

  /**
   * Creates signature for apply method.
   *
   * @param denot Functino denotation for which apply signature is returned.
   * @return Signature if denot is a function, None otherwise
   */
  private def toApplySignature(denot: SingleDenotation)(using Context): Option[Signature] = {
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
        case _ => Nil
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

    denot.info.stripPoly match
      case tpe: MethodType =>
        val paramss = toParamss(tpe)
        val typeParams = denot.info match
          case poly: PolyType => poly.paramNames.zip(poly.paramInfos).map { case (x, y) => x.show + y.show }
          case _ => Nil
        val (name, returnType) =
          if (symbol.isConstructor) then
            (symbol.owner.name.show, None)
          else
            (denot.name.show, Some(tpe.finalResultType.widenTermRefExpr.show))
        Some(Signatures.Signature(name, typeParams, paramss, returnType, docComment.map(_.mainDoc)))
      case other => None
  }

  /**
   * Creates signature for unapply method. It is different from apply one as it should not show function name,
   * return type and type parameters. Instead we show function in the following pattern (_$1: T1, _$2: T2, _$n: Tn),
   * where _$n is only present for synthetic product extractors such as case classes.
   * In rest of cases signature skips them resulting in pattern (T1, T2, T3, Tn)
   *
   * @param paramNames Parameter names for unapply final result type.
   *                   It non empty only when unapply returns synthetic product as for case classes.
   * @param paramTypes Parameter types for unapply final result type.
   * @return Signature if paramTypes is non empty, None otherwise
   */
  private def toUnapplySignature(paramNames: List[Name], paramTypes: List[Type])(using Context): Option[Signature] =
    val params = if paramNames.length == paramTypes.length then
      (paramNames zip paramTypes).map((name, info) => Param(name.show, info.show))
    else
      paramTypes.map(info => Param("", info.show))

    if params.nonEmpty then
      Some(Signature("", Nil, List(params), None))
    else
      None

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


