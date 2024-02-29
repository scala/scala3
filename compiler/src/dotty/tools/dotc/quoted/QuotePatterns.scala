package dotty.tools.dotc
package quoted

import dotty.tools.dotc.ast.TreeTypeMap
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Annotations.*
import dotty.tools.dotc.core.Constants.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.NameKinds.PatMatGivenVarName
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.TypeOps.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.reporting.IllegalVariableInPatternAlternative


import scala.collection.mutable

object QuotePatterns:
  import tpd.*

  /** Check for restricted patterns */
  def checkPattern(quotePattern: QuotePattern)(using Context): Unit = new tpd.TreeTraverser {
    def traverse(tree: Tree)(using Context): Unit = tree match {
      case _: SplicePattern =>
      case tdef: TypeDef if tdef.symbol.isClass =>
        val kind = if tdef.symbol.is(Module) then "objects" else "classes"
        report.error(em"Implementation restriction: cannot match $kind", tree.srcPos)
      case tree: NamedDefTree =>
        if tree.name.is(NameKinds.WildcardParamName) then
          report.warning(
            "Use of `_` for lambda in quoted pattern. Use explicit lambda instead or use `$_` to match any term.",
            tree.srcPos)
        if tree.name.isTermName && !tree.nameSpan.isSynthetic && tree.name != nme.ANON_FUN && tree.name.startsWith("$") then
          report.error("Names cannot start with $ quote pattern", tree.namePos)
        traverseChildren(tree)
      case _: Match =>
        report.error("Implementation restriction: cannot match `match` expressions", tree.srcPos)
      case _: Try =>
        report.error("Implementation restriction: cannot match `try` expressions", tree.srcPos)
      case _: Return =>
        report.error("Implementation restriction: cannot match `return` statements", tree.srcPos)
      case _ =>
        traverseChildren(tree)
    }

  }.traverse(quotePattern.body)

  /** Encode the quote pattern into an `unapply` that the pattern matcher can handle.
   *
   *  A quote pattern
   *  ```
   *  '{
   *    // type variables (QuotePattern.bindings Bind list)
   *    type t1 >: l1 <: b1
   *    ...
   *    type tn >: ln <: bn
   *    // pattern (QuotePattern.body)
   *    ... $x1: T1 ... ${ F(x2) }: T2 ... $f(a1: A1, ..., an: An): T3 ...
   *  } // (using quotes) (QuotePattern.quotes)
   *  ```
   *
   *  is transformed into the pattern
   *
   *  ```
   *  quotes
   *    .asInstanceOf[QuoteMatching] // scala.quoted.runtime.QuoteMatching
   *    .ExprMatch // or TypeMatch
   *    .unapply[
   *      KCons[t1 >: l1 <: b1, ...KCons[tn >: ln <: bn, KNil]...], // scala.quoted.runtime.{KCons, KNil}
   *      (T1, T2, (A1, ..., An) => T3, ...)
   *    ](
   *      '{
   *        type t1' >: l1' <: b1'
   *        ...
   *        type tn' >: ln' <: bn'
   *        // scala.quoted.runtime.Patterns.{patternHole, higherOrderHole}
   *        ... $patternHole[T1] ... $patternHole[T2] ... $higherOrderHole[T3](a1, ..., an) ...
   *       },
   *       quotes
   *    )
   *
   *   Here ti' is a `TypeDef` that represents `ti` in the (pickled) pattern body. The type bounds
   *   `>: l1' <: b1` of `ti'` are the same as the type bounds `>: l1 <: b1` replacing all references
   *   to `tj` with `tj'`.
   *  ```
   */
  def encode(quotePattern: QuotePattern)(using Context): UnApply =
    val quoteClass = if (quotePattern.body.isTerm) defn.QuotedExprClass else defn.QuotedTypeClass

    val matchModule = if quotePattern.body.isTerm then defn.QuoteMatching_ExprMatch else defn.QuoteMatching_TypeMatch
    val unapplySym = if quotePattern.body.isTerm then defn.QuoteMatching_ExprMatch_unapply else defn.QuoteMatching_TypeMatch_unapply
    val unapplyFun = quotePattern.quotes.asInstance(defn.QuoteMatchingClass.typeRef).select(matchModule).select(unapplySym)

    val typeBindingsTuple = tpd.hkNestedPairsTypeTree(quotePattern.bindings)

    val (splicePatterns, shape0) = splitQuotePattern(quotePattern.body)

    val shape1 =
      if quotePattern.bindings.isEmpty then shape0
      else
        val oldBindings = quotePattern.bindings.map(_.symbol)
        val newBindings = quotePattern.bindings.map { binding =>
          val sym = binding.symbol
          val typeSym = newSymbol(ctx.owner, sym.name, EmptyFlags, sym.info, NoSymbol, binding.span)
          typeSym.addAnnotation(defn.QuotedRuntimePatterns_patternTypeAnnot)
          for fromAbove <- sym.getAnnotation(defn.QuotedRuntimePatterns_fromAboveAnnot) do
            typeSym.addAnnotation(fromAbove)
          typeSym.asType
        }
        var newBindingsRefs = newBindings.map(_.typeRef)
        for newBinding <- newBindings do
          newBinding.info = newBinding.info.subst(oldBindings, newBindingsRefs)

        val patternTypes = newBindings.map(sym => TypeDef(sym).withSpan(sym.span))
        Block(patternTypes, shape0.subst(oldBindings, newBindings))

    val quotedShape =
      if (quotePattern.body.isTerm) tpd.Quote(shape1, Nil).select(nme.apply).appliedTo(quotePattern.quotes)
      else ref(defn.QuotedTypeModule_of.termRef).appliedToTypeTree(shape1).appliedTo(quotePattern.quotes)

    val givenTypes = quotePattern.bindings.map { binding =>
      val name = binding.symbol.name.toTypeName
      val nameOfSyntheticGiven = PatMatGivenVarName.fresh(name.toTermName)
      val tpe = defn.QuotedTypeClass.typeRef.appliedTo(binding.symbol.typeRef)
      val givenTypeSym = newPatternBoundSymbol(nameOfSyntheticGiven, tpe, binding.span, flags = Given)
      Bind(givenTypeSym, untpd.Ident(nme.WILDCARD).withType(tpe)).withSpan(binding.span)
    }

    val patterns = givenTypes ::: splicePatterns
    val patternTypes = patterns.map(_.tpe.widenTermRefExpr)

    val splicePat =
      if patterns.isEmpty then ref(defn.EmptyTupleModule.termRef)
      else if patterns.size <= Definitions.MaxTupleArity then
        val tupleNUnapply =
          ref(defn.TupleType(patterns.size).nn.typeSymbol.companionModule)
            .select(nme.unapply)
            .appliedToTypes(patternTypes)
        UnApply(tupleNUnapply, Nil, patterns, defn.tupleType(patternTypes))
      else
        val tupleXXLUnapplySeq = ref(defn.TupleXXL_unapplySeq)
        val unapply = UnApply(tupleXXLUnapplySeq, Nil, patterns, defn.tupleType(patternTypes))
        Typed(unapply, TypeTree(defn.TupleXXLClass.typeRef))

    val patType =
      val quotedTypes =
        quotePattern.bindings.map(givenType => defn.QuotedTypeClass.typeRef.appliedTo(givenType.symbol.typeRef))
      val quotedExprs =
        splicePatterns.map(_.tpe.widenTermRefExpr)
      defn.tupleType(quotedTypes :::quotedExprs)

    UnApply(
      fun = unapplyFun.appliedToTypeTrees(typeBindingsTuple :: TypeTree(patType) :: Nil),
      implicits = quotedShape :: Nil,
      patterns = splicePat :: Nil,
      quotePattern.tpe)

  /** Split a typed quoted pattern into the contents of its splices and replace them with place holders.
   *
   *  A quote pattern
   *  ```
   *  case '${
   *    val a: T = ???
   *    List[T](
   *       $x,
   *       ${Expr(y)},
   *       $f(a))
   *    )
   *  } => ...
   *  ```
   *  will return
   *  ```
   *  (
   *    List(
   *      <x: Expr[T]>: Tree,
   *      <Expr(y): Expr[T]>: Tree,
   *      <f: Expr[T => T]>: Tree)
   *    <'{
   *       val a: T = ???
   *       List[T](
   *         scala.quoted.runtime.Patterns.patternHole[T],
   *         scala.quoted.runtime.Patterns.patternHole[T],
   *         scala.quoted.runtime.Patterns.higherOrderHole[T](a)
   *       )
   *    }>: Tree,
   *  )
   *  ```
   */
  private def splitQuotePattern(body: Tree)(using Context): (List[Tree], Tree) = {
    val patBuf = new mutable.ListBuffer[Tree]
    val shape = new tpd.TreeMap {
      override def transform(tree: Tree)(using Context) = tree match {
        case Typed(splice @ SplicePattern(pat, Nil), tpt) if !tpt.tpe.derivesFrom(defn.RepeatedParamClass) =>
          transform(tpt) // Collect type bindings
          transform(splice)
        case SplicePattern(pat, args) =>
          val patType = pat.tpe.widen
          val patType1 = patType.translateFromRepeated(toArray = false)
          val pat1 = if (patType eq patType1) pat else pat.withType(patType1)
          patBuf += pat1
          if args.isEmpty then ref(defn.QuotedRuntimePatterns_patternHole.termRef).appliedToType(tree.tpe).withSpan(tree.span)
          else ref(defn.QuotedRuntimePatterns_higherOrderHole.termRef).appliedToType(tree.tpe).appliedTo(SeqLiteral(args, TypeTree(defn.AnyType))).withSpan(tree.span)
        case _ =>
          super.transform(tree)
      }
    }.transform(body)
    (patBuf.toList, shape)
  }


  /** Decodes an encoded pattern into a QuotePattern.
   *
   *  See the documentation of `encode`, this does the opposite transformation.
   */
  def decode(tree: UnApply)(using Context): QuotePattern =
    val (fun, implicits, patternTuple) = (tree: @unchecked) match
      case UnApply(fun, implicits, patternTuple :: Nil) => (fun, implicits, patternTuple)
    val patterns = patternTuple match
      case _: Ident => Nil // EmptyTuple
      case UnApply(_, _, patterns) => patterns // TupleN
      case Typed(UnApply(_, _, patterns), _) => patterns // TupleXXL
    val shape = (implicits: @unchecked) match
      case Apply(Select(Quote(shape, _), _), _) :: Nil => shape
      case List(Apply(TypeApply(_, shape :: Nil), _)) => shape
    fun match
      // <quotes>.asInstanceOf[QuoteMatching].{ExprMatch,TypeMatch}.unapply[<typeBindings>, <resTypes>]
      case TypeApply(Select(Select(TypeApply(Select(quotes, _), _), _), _), typeBindings :: resTypes :: Nil) =>
        val bindings = unrollBindings(typeBindings)
        val addPattenSplice = new TreeMap {
          private val patternIterator = patterns.iterator.filter {
            case pat: Bind => !pat.symbol.name.is(PatMatGivenVarName)
            case _ => true
          }
          override def transform(tree: tpd.Tree)(using Context): tpd.Tree = tree match
            case TypeApply(patternHole, _) if patternHole.symbol == defn.QuotedRuntimePatterns_patternHole =>
              cpy.SplicePattern(tree)(patternIterator.next(), Nil)
            case Apply(patternHole, SeqLiteral(args, _) :: Nil) if patternHole.symbol == defn.QuotedRuntimePatterns_higherOrderHole =>
              cpy.SplicePattern(tree)(patternIterator.next(), args)
            case _ => super.transform(tree)
        }
        val body = addPattenSplice.transform(shape) match
          case block @ Block((tdef: TypeDef) :: rest, expr) if tdef.symbol.hasAnnotation(defn.QuotedRuntimePatterns_patternTypeAnnot) =>
            val (tdefs, stats) = rest.span {
              case tdef: TypeDef => tdef.symbol.hasAnnotation(defn.QuotedRuntimePatterns_patternTypeAnnot)
              case _ => false
            }
            val shapeBindingSyms = tdef.symbol :: tdefs.map(_.symbol)
            for (binding, shapeBinding) <- bindings.zip(shapeBindingSyms) do
              if shapeBinding.hasAnnotation(defn.QuotedRuntimePatterns_fromAboveAnnot) then
                binding.symbol.addAnnotation(defn.QuotedRuntimePatterns_fromAboveAnnot)
            val body1 = if stats.isEmpty then expr else cpy.Block(block)(stats, expr)
            body1.subst(shapeBindingSyms, bindings.map(_.symbol))
          case body => body
        cpy.QuotePattern(tree)(bindings, body, quotes)

  private def unrollBindings(tree: Tree)(using Context): List[Tree] = tree match
    case AppliedTypeTree(tupleN, bindings) if defn.isTupleClass(tupleN.symbol) => bindings // TupleN, 1 <= N <= 22
    case AppliedTypeTree(_, head :: tail :: Nil) => head :: unrollBindings(tail) // KCons or *:
    case _ => Nil // KNil or EmptyTuple
