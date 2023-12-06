package dotty.tools.dotc
package util

import dotty.tools.dotc.ast.NavigateAST
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.NameOps.*

import ast.Trees.*
import ast.tpd
import core.Contexts.*
import core.Denotations.{SingleDenotation, Denotation}
import core.Flags
import core.Names.*
import core.NameKinds
import core.Types.*
import core.Symbols.{NoSymbol, isLocalToBlock}
import interactive.Interactive
import util.Spans.Span
import reporting.*


object Signatures {

  /**
   * Represent a method signature.
   *
   * @param name       The name of the method
   * @param tparams    The type parameters and their bounds
   * @param paramss    The parameter lists of this method
   * @param returnType The return type of this method, if this is not a constructor.
   * @param doc        The documentation for this method.
   * @param denot      The function denotation
   */
  case class Signature(
    name: String,
    tparams: List[String],
    paramss: List[List[Param]],
    returnType: Option[String],
    doc: Option[String] = None,
    denot: Option[SingleDenotation] = None
  )

  /**
   * Represent a method's parameter.
   *
   * @param name       The name of the parameter
   * @param tpe        The type of the parameter
   * @param doc        The documentation of this parameter
   * @param isImplicit Is this parameter implicit?
   */
  case class Param(name: String, tpe: String, doc: Option[String] = None, isImplicit: Boolean = false, isReordered: Boolean = false) {
    def show: String = if name.nonEmpty && !isReordered then s"$name: $tpe"
    else if name.nonEmpty then s"[$name: $tpe]"
    else tpe
  }

  /**
   * Extract (current parameter index, function index, functions) method call for given position.
   *
   * @param path The path to the function application
   * @param pos  The position of the cursor
   *
   * @return     A triple containing the index of the parameter being edited, the index of function
   *         being called, the list of overloads of this function).
   */
  def signatureHelp(path: List[tpd.Tree], pos: Span)(using Context): (Int, Int, List[Signature]) =
    computeSignatureHelp(path, pos)

  /**
   * Computes call info (current parameter index, function index, functions) for a method call.
   *
   * @param path The path to the function application
   * @param span The position of the cursor
   *
   * @return A triple containing the index of the parameter being edited, the index of the function
   *         being called, the list of overloads of this function).
   */
  def computeSignatureHelp(path: List[tpd.Tree], span: Span)(using Context): (Int, Int, List[Signature]) =
    findEnclosingApply(path, span) match
      case Apply(fun, params) => applyCallInfo(span, params, fun)
      case UnApply(fun, _, patterns) => unapplyCallInfo(span, fun, patterns)
      case appliedTypeTree @ AppliedTypeTree(_, types) => appliedTypeTreeCallInfo(appliedTypeTree, types)
      case tp @ TypeApply(fun, types) => applyCallInfo(span, types, fun, true)
      case _ => (0, 0, Nil)


  def isEnclosingApply(tree: tpd.Tree, span: Span)(using Context): Boolean =
    tree match
      case apply @ Apply(fun, _) => !fun.span.contains(span) && isValid(apply)
      case unapply @ UnApply(fun, _, _) =>
        !fun.span.contains(span) && !ctx.definitions.isFunctionNType(tree.tpe) // we want to show tuples in unapply
      case typeTree @ AppliedTypeTree(fun, _) => !fun.span.contains(span) && isValid(typeTree)
      case typeApply @ TypeApply(fun, _) => !fun.span.contains(span) && isValid(typeApply)
      case _ => false


  /**
   * Finds enclosing application from given `path` for `span`.
   *
   * @param path The path to the function application
   * @param span The position of the cursor
   *
   * @return Tree which encloses closest application containing span.
   *         In case if cursor is pointing on closing parenthesis and
   *         next subsequent application exists, it returns the latter
   */
  private def findEnclosingApply(path: List[tpd.Tree], span: Span)(using Context): tpd.Tree =
    import tpd.TreeOps

    val filteredPath = path.filter:
      case block @ Block(stats, expr) =>
        block.existsSubTree(tree => isEnclosingApply(tree, span) && tree.span.contains(span))
      case other => isEnclosingApply(other, span)

    filteredPath match
      case Nil => tpd.EmptyTree
      case tpd.Block(stats, expr) :: _ => // potential block containing lifted args

        val enclosingFunction = stats.collectFirst:
          case defdef: tpd.DefDef if defdef.rhs.span.contains(span) => defdef

        val enclosingTree = enclosingFunction.getOrElse(expr)
        findEnclosingApply(Interactive.pathTo(enclosingTree, span), span)

      case direct :: enclosing :: _ if isClosingSymbol(direct.source(span.end -1)) => enclosing
      case direct :: _ => direct


  private def isClosingSymbol(ch: Char) = ch == ')' || ch == ']'

  /**
   * Extracts call information for applied type tree:
   *
   * @param types Currently applied function type parameters
   * @param fun   Function tree which is being applied
   */
  private def appliedTypeTreeCallInfo(
    fun: tpd.Tree,
    types: List[tpd.Tree]
  )(using Context): (Int, Int, List[Signature]) =
    val typeName = fun.symbol.name.show
    val typeParams = fun.symbol.typeRef.typeParams.map(_.paramName.show)
    val denot = fun.denot.asSingleDenotation
    val activeParameter = (types.length - 1) max 0

    val signature = Signature(typeName, typeParams, Nil, Some(typeName) , None, Some(denot))
    (activeParameter, 0, List(signature))

  /**
   * Extracts call information for a function application and type application.
   *
   * @param span        The position of the cursor
   * @param params      Current function parameters
   * @param fun         Function tree which is being applied
   * @param isTypeApply Is a type application
   * @return A triple containing the index of the parameter being edited, the index of the function
   *         being called, the list of overloads of this function).
   */
  private def applyCallInfo(
    span: Span,
    params: List[tpd.Tree],
    fun: tpd.Tree,
    isTypeApply: Boolean = false
  )(using Context): (Int, Int, List[Signature]) =
    def treeQualifier(tree: tpd.Tree): tpd.Tree = tree match
        case Apply(qual, _) => treeQualifier(qual)
        case TypeApply(qual, _) => treeQualifier(qual)
        case AppliedTypeTree(qual, _) => treeQualifier(qual)
        case Select(qual, _) => qual
        case _ => tree

    val (alternativeIndex, alternatives) = fun.tpe match
      case err: ErrorType =>
        val (alternativeIndex, alternatives) = alternativesFromError(err, params) match
          // if we have no alternatives from error, we have to fallback to function denotation
          // Check `partialyFailedCurriedFunctions` test for example
          case (_, Nil) =>
            val denot = fun.denot
            if denot.exists then
              (0, List(denot.asSingleDenotation))
            else
              (0, Nil)
          case other => other
        (alternativeIndex, alternatives)
      case _ =>
        val funSymbol = fun.symbol
        val alternatives = if funSymbol.isLocalToBlock then List(funSymbol.denot) else
          funSymbol.owner.info.member(funSymbol.name).alternatives
        val alternativeIndex = alternatives.map(_.symbol).indexOf(funSymbol) max 0
        (alternativeIndex, alternatives)

    if alternativeIndex < alternatives.length then
      val alternativeSymbol = alternatives(alternativeIndex).symbol
      val paramssListIndex = findParamssIndex(fun, alternatives(alternativeIndex))

      val previousArgs = alternativeSymbol.paramSymss.take(paramssListIndex).foldLeft(0)(_ + _.length)
      val previousTypeParams = if !isTypeApply then alternativeSymbol.paramSymss.flatten.filter(_.isType).length else 0

      val untpdPath: List[untpd.Tree] = NavigateAST
        .untypedPath(fun, false).collect { case untpdTree: untpd.Tree => untpdTree }

      val untpdArgs = untpdPath match
        case Ident(_) :: New(_) :: Select(_, name) :: untpd.Apply(_, args) :: _ if name.isConstructorName => args
        case _ :: untpd.Apply(_, args) :: _ => args
        case _ :: untpd.TypeApply(_, args) :: _ => args
        case _ => Nil

      val currentParamsIndex = untpdArgs.indexWhere(_.span.contains(span)) match
        case -1 if untpdArgs.isEmpty => 0
        case -1 =>
          commaIndex(untpdArgs, span) match
            // comma is after CURSOR, so we are in parameter a
            case Some(index) if index <= span.start => untpdArgs.takeWhile(_.span.start < span.start).length
            // comma is before CURSOR, so we are in parameter b
            case Some(index) => untpdArgs.takeWhile(_.span.start < span.start).length - 1
            // we are either in first or last parameter
            case None =>
              if untpdArgs.head.span.start >= span.start then 0
              else untpdArgs.length - 1 max 0

        // special case if we pass more arguments than function has parameters
        case n => n min (alternativeSymbol.paramSymss(paramssListIndex).length - 1)

      val firstOrderedParams =
        val originalParams = params.map(_.span)
        val untpdParams = untpdArgs.map(_.span)
        originalParams.zip(untpdParams).takeWhile((original, untpd) => original == untpd).length

      val reorderedNamedArgs = untpdArgs.drop(firstOrderedParams).takeWhile(_.span.start <= span.start).collect:
        case namedArg: untpd.NamedArg => namedArg.name.show

      val namedArgsAfterCursor = untpdArgs.drop(currentParamsIndex + 1).dropWhile(_.span.start <= span.start).collect:
        case namedArg: untpd.NamedArg => namedArg.name.show

      val pre = treeQualifier(fun)
      val alternativesWithTypes = alternatives.map(_.asSeenFrom(pre.tpe.widenTermRefExpr))
      val alternativeSignatures = alternativesWithTypes
        .flatMap(toApplySignature(_, reorderedNamedArgs, namedArgsAfterCursor, firstOrderedParams))

      val finalParamIndex = currentParamsIndex + previousArgs + previousTypeParams
      (finalParamIndex, alternativeIndex, alternativeSignatures)
    else
      (0, 0, Nil)

  /** Parser ignores chars between arguments, we have to manually find the index of comma
   *  @param untpdArgs List of applied untyped arguments
   *  @param span      The position of the cursor
   *
   *  @return None if we are in first or last parameter, comma index otherwise
   */
  private def commaIndex(untpdArgs: List[untpd.Tree], span: Span)(using Context): Option[Int] =
    val previousArgIndex = untpdArgs.lastIndexWhere(_.span.end < span.start)
    for
      previousArg <- untpdArgs.lift(previousArgIndex)
      nextArg <- untpdArgs.lift(previousArgIndex + 1)
    yield
      val text = ctx.source.content.slice(previousArg.span.end, nextArg.span.start)
      text.indexOf(',') + previousArg.span.end

  /**
   * Extracts call informatioin for function in unapply context.
   *
   * @param span   The position of the cursor
   * @param params Current function parameters
   * @param fun    Unapply function tree
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
    val denot = fun.denot.mapInfo(_ => resultType)

    val paramTypes = extractParamTypess(resultType, denot, patterns.size).flatten.map(stripAllAnnots)
    val paramNames = extractParamNamess(resultType, denot).flatten

    val activeParameter = unapplyParameterIndex(patterns, span, paramTypes.length)
    val unapplySignature = toUnapplySignature(denot.asSingleDenotation, paramNames, paramTypes).toList

    (activeParameter, 0, unapplySignature)


  private def isUnapplySeq(denot: Denotation)(using Context): Boolean =
    denot.name == core.Names.termName("unapplySeq")

  /**
   * Extract parameter names from `resultType` only if `resultType` is case class and `denot` is synthetic.
   *
   * @param resultType Function result type
   * @param denot      Function denotation
   *
   * @return List of lists of names of parameters if `resultType` is case class without overriden unapply
   */
  private def extractParamNamess(resultType: Type, denot: Denotation)(using Context): List[List[Name]] =
    if resultType.typeSymbol.flags.is(Flags.CaseClass) && denot.symbol.flags.is(Flags.Synthetic) then
      resultType.typeSymbol.primaryConstructor.paramInfo.paramNamess
    else
      Nil

  /**
   * Extract parameter types from `resultType` in unapply context.
   *
   * @param resultType   Function result type
   * @param denot        Function denotation
   * @param patternsSize Number of pattern trees present in function tree
   *
   * @return List of lists of types present in unapply clause
   */
  private def extractParamTypess(
    resultType: Type,
    denot: Denotation,
    patternsSize: Int
  )(using Context): List[List[Type]] =
    resultType match
      // unapply(_$1: Any): CustomClass
      case ref: TypeRef if !ref.symbol.isPrimitiveValueClass => mapOptionLessUnapply(ref, patternsSize, isUnapplySeq(denot))
      // unapply(_$1: Any): Option[T[_]]
      case AppliedType(TypeRef(_, cls), (appliedType @ AppliedType(tycon, args)) :: Nil)
          if (cls == ctx.definitions.OptionClass || cls == ctx.definitions.SomeClass) =>
        tycon match
          // unapply[T](_$1: Any): Option[(T1, T2 ... Tn)]
          case typeRef: TypeRef if ctx.definitions.isTupleClass(typeRef.symbol) => List(args)
          case _ => List(List(appliedType))
      // unapply[T](_$1: Any): CustomClass[T]
      case appliedType: AppliedType => mapOptionLessUnapply(appliedType, patternsSize, isUnapplySeq(denot))
      case _ => Nil

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

  /**
   * Checks if tree is valid for signatureHelp. Skipped trees are either tuple type or function type
   *
   * @param tree tree to validate
   */
  private def isValid(tree: tpd.Tree)(using Context): Boolean =
    !ctx.definitions.isTupleNType(tree.tpe) && !ctx.definitions.isFunctionNType(tree.tpe)

  /**
   * Get unapply method result type omiting unknown types and another method calls.
   *
   * @param fun Unapply tree
   *
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
   *  @param resultType   Final result type for unapply
   *  @param patternCount Currently applied patterns to unapply function
   *  @param isUnapplySeq true if unapply name equals "unapplySeq", false otherwise
   *
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

    val availableExtractors =
      if isUnapplySeq && dropMethod.exists then
        List(dropMethod)
      else if getMethod.exists && patternCount <= 1 then
        List(getMethod)
      else
        productAccessors

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
   * @param denot Function denotation for which apply signature is returned.
   *
   * @return Signature if denot is a function, None otherwise
   */
  private def toApplySignature(
    denot: SingleDenotation,
    namedParamsBeforeCursor: List[String],
    namedParamsAfterCursor: List[String],
    firstOrderedParams: Int
  )(using Context): Option[Signature] = {
    val symbol = denot.symbol
    val docComment = ParsedComment.docOf(symbol)

    def isDummyImplicit(res: MethodType): Boolean =
      res.resultType.isParameterless &&
        res.isImplicitMethod &&
        res.paramInfos.forall(info =>
          info.classSymbol.derivesFrom(ctx.definitions.DummyImplicitClass))

    def toParamss(tp: Type)(using Context): List[List[Param]] =
      val rest = tp.resultType match
        case res: MethodType => if isDummyImplicit(res) then Nil else toParamss(res)
        case _ => Nil

      val currentParams = (tp.paramNamess, tp.paramInfoss) match
        case (params :: _, infos :: _) => params zip infos
        case _ => Nil

      val params = currentParams.map: (name, info) =>
        Signatures.Param(
          name.show,
          info.widenTermRefExpr.show,
          docComment.flatMap(_.paramDoc(name)),
          isImplicit = tp.isImplicitMethod,
        )

      val ordered = params.take(firstOrderedParams)
      val reorderedParamsBeforeCursor = namedParamsBeforeCursor.flatMap: name =>
        params.find(_.name == name)

      val (remainingNamedUnordered, remaining) = params
        .diff(reorderedParamsBeforeCursor).diff(ordered)
        .partition(p => namedParamsAfterCursor.contains(p.name))

      val remainingNamed = remainingNamedUnordered.sortBy(p => namedParamsAfterCursor.indexOf(p.name))
      val reorderedArgs = (reorderedParamsBeforeCursor ++ remaining ++ remainingNamed)
        .map(_.copy(isReordered = reorderedParamsBeforeCursor.nonEmpty))

      (ordered ++ reorderedArgs) :: rest

    def isSyntheticEvidence(name: String) =
      if !name.startsWith(NameKinds.ContextBoundParamName.separator) then false else
        symbol.paramSymss.flatten.find(_.name.show == name).exists(_.flags.is(Flags.Implicit))

    denot.info.stripPoly match
      case tpe: (MethodType | AppliedType | TypeRef | TypeParamRef) =>
        val paramss = toParamss(tpe).map(_.filterNot(param => isSyntheticEvidence(param.name)))
        val evidenceParams = (tpe.paramNamess.flatten zip tpe.paramInfoss.flatten).flatMap {
          case (name, AppliedType(tpe, (ref: TypeParamRef) :: _)) if isSyntheticEvidence(name.show) =>
            Some(ref.paramName -> tpe)
          case _ => None
        }

        val typeParams = denot.info match
          case poly: PolyType =>
            val tparams = poly.paramNames.zip(poly.paramInfos)
            tparams.map { (name, info) =>
              evidenceParams.find((evidenceName: TypeName, _: Type) => name == evidenceName).flatMap {
                case (_, tparam) => tparam.show.split('.').lastOption
              } match {
                case Some(evidenceTypeName) => s"${name.show}: ${evidenceTypeName}"
                case None => name.show + info.show
              }
            }
          case _ => Nil
        val (name, returnType) =
          if (symbol.isConstructor) then
            (symbol.owner.name.show, None)
          else
            (denot.name.show, Some(tpe.finalResultType.widenTermRefExpr.show))
        Some(Signatures.Signature(name, typeParams, paramss, returnType, docComment.map(_.mainDoc), Some(denot)))
      case other => None
  }

  /**
   * Creates signature for unapply method. It is different from apply one as it should not show function name,
   * return type and type parameters. Instead we show function in the following pattern (_$1: T1, _$2: T2, _$n: Tn),
   * where _$n is only present for synthetic product extractors such as case classes.
   * In rest of cases signature skips them resulting in pattern (T1, T2, T3, Tn)
   *
   * @param denot      Unapply denotation
   * @param paramNames Parameter names for unapply final result type.
   *                     It non empty only when unapply returns synthetic product as for case classes.
   * @param paramTypes Parameter types for unapply final result type.
   *
   * @return Signature if paramTypes is non empty, None otherwise
   */
  private def toUnapplySignature(denot: SingleDenotation, paramNames: List[Name], paramTypes: List[Type])(using Context): Option[Signature] =
    val params = if paramNames.length == paramTypes.length then
      (paramNames zip paramTypes).map((name, info) => Param(name.show, info.show))
    else
      paramTypes.map(info => Param("", info.show))

    if params.nonEmpty then
      Some(Signature("", Nil, List(params), None, None, Some(denot)))
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
   * @param tree           The tree to inspect.
   * @param denot          Denotation of function we are trying to apply
   * @param alreadyCurried Number of subsequent Apply trees before current tree
   *
   * @return The number of parameters that are passed.
   */
  private def findParamssIndex(tree: tpd.Tree, denot: SingleDenotation, alreadyCurried: Int = 0)(using Context): Int =
    tree match
      case Apply(fun, params) => findParamssIndex(fun, denot, alreadyCurried + 1)
      case _ => alreadyCurried

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
   *
   * @return A pair composed of the index of the best alternative (0 if no alternatives
   *         were found), and the list of alternatives.
   */
  private def alternativesFromError(err: ErrorType, params: List[tpd.Tree])(using Context): (Int, List[SingleDenotation]) = {
    val alternatives =
      err.msg match
        case msg: AmbiguousOverload  => msg.alternatives
        case msg: NoMatchingOverload => msg.alternatives
        case _                       => Nil

    val userParamsTypes = params.map(_.tpe)

    // Assign a score to each alternative (how many parameters are correct so far), and
    // use that to determine what is the current active signature.
    val alternativesScores = alternatives.map { alt =>
      alt.info.stripPoly match
        case tpe: MethodType =>
          userParamsTypes.zip(tpe.paramInfos).takeWhile{ case (t0, t1) => t0 <:< t1 }.size
        case _ => 0
    }
    val bestAlternative =
      if (alternativesScores.isEmpty) 0
      else alternativesScores.zipWithIndex.maxBy(_._1)._2

    (bestAlternative, alternatives)
  }
}


