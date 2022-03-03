package dotty.tools.dotc
package transform

import core._
import Decorators._
import Flags._
import Types._
import Contexts._
import Symbols._
import Constants._
import ast.Trees._
import ast.TreeTypeMap
import SymUtils._
import NameKinds._
import dotty.tools.dotc.ast.tpd

import scala.collection.mutable
import dotty.tools.dotc.core.Annotations._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.quoted._
import dotty.tools.dotc.transform.TreeMapWithStages._
import dotty.tools.dotc.typer.Inliner

import scala.annotation.constructorOnly


/** Translates quoted terms and types to `unpickleExpr` or `unpickleType` method calls.
 *
 *  Transforms top level quote
 *   ```
 *   '{ ...
 *      val x1 = ???
 *      val x2 = ???
 *      ...
 *      ${ ... '{ ... x1 ... x2 ...} ... }
 *      ...
 *    }
 *    ```
 *  to
 *    ```
 *     unpickleExpr(
 *       pickled = [[ // PICKLED TASTY
 *         ...
 *         val x1 = ???
 *         val x2 = ???
 *         ...
 *         Hole(<i> | x1, x2)
 *         ...
 *       ]],
 *       typeHole = (idx: Int, args: List[Any]) => idx match {
 *         case 0 => ...
 *       },
 *       termHole = (idx: Int, args: List[Any], qctx: Quotes) => idx match {
 *         case 0 => ...
 *         ...
 *         case <i> =>
 *           val x1$1 = args(0).asInstanceOf[Expr[T]]
 *           val x2$1 = args(1).asInstanceOf[Expr[T]] // can be asInstanceOf[Type[T]]
 *            ...
 *           { ... '{ ... ${x1$1} ... ${x2$1} ...} ... }
 *       },
 *     )
 *    ```
 *  and then performs the same transformation on `'{ ... ${x1$1} ... ${x2$1} ...}`.
 *
 */
class PickleQuotes extends MacroTransform {
  import PickleQuotes._
  import tpd._

  override def phaseName: String = PickleQuotes.name

  override def description: String = PickleQuotes.description

  override def allowsImplicitSearch: Boolean = true

  override def checkPostCondition(tree: Tree)(using Context): Unit =
    tree match {
      case tree: RefTree if !Inliner.inInlineMethod =>
        assert(!tree.symbol.isQuote)
        assert(!tree.symbol.isExprSplice)
      case _ : TypeDef =>
        assert(!tree.symbol.hasAnnotation(defn.QuotedRuntime_SplicedTypeAnnot),
          s"${tree.symbol} should have been removed by PickledQuotes because it has a @quoteTypeTag")
      case _ =>
    }

  override def run(using Context): Unit =
    if (ctx.compilationUnit.needsQuotePickling) super.run(using freshStagingContext)

  protected def newTransformer(using Context): Transformer = new Transformer {
    override def transform(tree: tpd.Tree)(using Context): tpd.Tree =
      new QuoteReifier(null, new mutable.HashMap[Symbol, Tree => Tree], new Embedded, ctx.owner)(ctx).transform(tree)
  }

  /** The main transformer class
   *  @param  outer      the next outer reifier, null is this is the topmost transformer
   *  @param  embedded   a list of embedded quotes (if in a splice) or splices (if in a quote)
   *  @param  owner      the owner in the destination lifted lambda
   *  @param  capturers  register a reference defined in a quote but used in another quote nested in a splice.
   *                     Returns a version of the reference that needs to be used in its place.
   *                     '{
   *                       val x = ???
   *                       ${ ... '{ ... x ... } ... }
   *                     }
   *                     Eta expanding the `x` in `${ ... '{ ... x ... } ... }` will return a `${x$1}` for which the `x$1`
   *                     be created by some outer reifier.
   *                     This transformation is only applied to definitions at staging level 1.
   *                     See `isCaptured`.
   */
  private class QuoteReifier(outer: QuoteReifier | Null, capturers: mutable.HashMap[Symbol, Tree => Tree],
                             val embedded: Embedded, val owner: Symbol)(@constructorOnly ictx: Context) extends TreeMapWithStages(ictx) { self =>

    import StagingContext._

    /** A nested reifier for a quote (if `isQuote = true`) or a splice (if not) */
    def nested(isQuote: Boolean)(using Context): QuoteReifier = {
      val nestedEmbedded = if (level > 1 || (level == 1 && isQuote)) embedded else new Embedded
      new QuoteReifier(this, capturers, nestedEmbedded, ctx.owner)(ctx)
    }

    /** Split `body` into a core and a list of embedded splices.
     *  Then if inside a splice, make a hole from these parts.
     *  If outside a splice, generate a call tp `scala.quoted.Unpickler.unpickleType` or
     *  `scala.quoted.Unpickler.unpickleExpr` that matches `tpe` with
     *  core and splices as arguments.
     */
    override protected def transformQuotation(body: Tree, quote: Apply)(using Context): Tree = {
      val isType = quote.symbol eq defn.QuotedTypeModule_of
      if (level > 0) {
        val body1 = nested(isQuote = true).transform(body)(using quoteContext)
        super.transformQuotation(body1, quote)
      }
      else {
        val (body1, splices) = nested(isQuote = true).splitQuote(body)(using quoteContext)
        if (level == 0) {
          val body2 =
            if (body1.isType) body1
            else Inlined(Inliner.inlineCallTrace(ctx.owner, quote.sourcePos), Nil, body1)
          pickledQuote(quote, body2, splices, body.tpe, isType).withSpan(quote.span)
        }
        else
          body
      }
    }

    private def pickledQuote(quote: Apply, body: Tree, splices: List[Tree], originalTp: Type, isType: Boolean)(using Context) = {
      /** Encode quote using Reflection.Literal
       *
       *  Generate the code
       *  ```scala
       *    qctx => qctx.reflect.TreeMethods.asExpr(
       *      qctx.reflect.Literal.apply(x$1.reflect.Constant.<typeName>.apply(<literalValue>))
       *    ).asInstanceOf[scala.quoted.Expr[<body.type>]]
       *  ```
       *  this closure is always applied directly to the actual context and the BetaReduce phase removes it.
       */
      def pickleAsLiteral(lit: Literal) = {
        val exprType = defn.QuotedExprClass.typeRef.appliedTo(body.tpe)
        val lambdaTpe = MethodType(defn.QuotesClass.typeRef :: Nil, exprType)
        def mkConst(ts: List[Tree]) = {
          val reflect = ts.head.select("reflect".toTermName)
          val typeName = body.tpe.typeSymbol.name
          val literalValue =
            if lit.const.tag == Constants.NullTag || lit.const.tag == Constants.UnitTag then Nil
            else List(body)
          val constant = reflect.select(s"${typeName}Constant".toTermName).select(nme.apply).appliedToTermArgs(literalValue)
          val literal = reflect.select("Literal".toTermName).select(nme.apply).appliedTo(constant)
          reflect.select("TreeMethods".toTermName).select("asExpr".toTermName).appliedTo(literal).asInstance(exprType)
        }
        Lambda(lambdaTpe, mkConst).withSpan(body.span)
      }

      /** Encode quote using Reflection.Literal
       *
       *  Generate the code
       *  ```scala
       *    qctx => scala.quoted.ToExpr.{BooleanToExpr,ShortToExpr, ...}.apply(<literalValue>)(qctx)
       *  ```
       *  this closure is always applied directly to the actual context and the BetaReduce phase removes it.
       */
      def liftedValue(lit: Literal, lifter: Symbol) =
        val exprType = defn.QuotedExprClass.typeRef.appliedTo(body.tpe)
        val lambdaTpe = MethodType(defn.QuotesClass.typeRef :: Nil, exprType)
        def mkToExprCall(ts: List[Tree]) =
          ref(lifter).appliedToType(originalTp).select(nme.apply).appliedTo(lit).appliedTo(ts.head)
        Lambda(lambdaTpe, mkToExprCall).withSpan(body.span)

      def pickleAsValue(lit: Literal) = {
        // TODO should all constants be pickled as Literals?
        // Should examime the generated bytecode size to decide and performance
        lit.const.tag match {
          case Constants.NullTag => pickleAsLiteral(lit)
          case Constants.UnitTag => pickleAsLiteral(lit)
          case Constants.BooleanTag => liftedValue(lit, defn.ToExprModule_BooleanToExpr)
          case Constants.ByteTag => liftedValue(lit, defn.ToExprModule_ByteToExpr)
          case Constants.ShortTag => liftedValue(lit, defn.ToExprModule_ShortToExpr)
          case Constants.IntTag => liftedValue(lit, defn.ToExprModule_IntToExpr)
          case Constants.LongTag => liftedValue(lit, defn.ToExprModule_LongToExpr)
          case Constants.FloatTag => liftedValue(lit, defn.ToExprModule_FloatToExpr)
          case Constants.DoubleTag => liftedValue(lit, defn.ToExprModule_DoubleToExpr)
          case Constants.CharTag => liftedValue(lit, defn.ToExprModule_CharToExpr)
          case Constants.StringTag => liftedValue(lit, defn.ToExprModule_StringToExpr)
        }
      }

      /** Encode quote using QuoteUnpickler.{unpickleExpr, unpickleType}
       *
       *  Generate the code
       *  ```scala
       *    qctx => qctx.asInstanceOf[QuoteUnpickler].<unpickleExpr|unpickleType>[<type>](
       *      <pickledQuote>,
       *      <typeHole>,
       *      <termHole>,
       *    )
       *  ```
       *  this closure is always applied directly to the actual context and the BetaReduce phase removes it.
       */
      def pickleAsTasty() = {
        val pickleQuote = PickledQuotes.pickleQuote(body)
        val pickledQuoteStrings = pickleQuote match
          case x :: Nil => Literal(Constant(x))
          case xs => liftList(xs.map(x => Literal(Constant(x))), defn.StringType)

        // TODO split holes earlier into types and terms. This all holes in each category can have consecutive indices
        val (typeSplices, termSplices) = splices.zipWithIndex.partition { case (splice, _) =>
          splice.tpe match
            case defn.FunctionOf(_, res, _, _) => res.typeSymbol == defn.QuotedTypeClass
        }

        // This and all closures in typeSplices are removed by the BetaReduce phase
        val typeHoles =
          if typeSplices.isEmpty then Literal(Constant(null)) // keep pickled quote without splices as small as possible
          else
            Lambda(
              MethodType(
                List(defn.IntType, defn.SeqType.appliedTo(defn.AnyType)),
                defn.QuotedTypeClass.typeRef.appliedTo(WildcardType)),
              args => {
                val cases = typeSplices.map { case (splice, idx) =>
                  CaseDef(Literal(Constant(idx)), EmptyTree, splice.select(nme.apply).appliedTo(args(1)))
                }
                Match(args(0).annotated(New(ref(defn.UncheckedAnnot.typeRef))), cases)
              }
            )

        // This and all closures in termSplices are removed by the BetaReduce phase
        val termHoles =
          if termSplices.isEmpty then Literal(Constant(null)) // keep pickled quote without splices as small as possible
          else
            Lambda(
              MethodType(
                List(defn.IntType, defn.SeqType.appliedTo(defn.AnyType), defn.QuotesClass.typeRef),
                defn.QuotedExprClass.typeRef.appliedTo(defn.AnyType)),
              args => {
                val cases = termSplices.map { case (splice, idx) =>
                  val defn.FunctionOf(_, defn.FunctionOf(qctxType :: _, _, _, _), _, _) = splice.tpe
                  val rhs = splice.select(nme.apply).appliedTo(args(1)).select(nme.apply).appliedTo(args(2).asInstance(qctxType))
                  CaseDef(Literal(Constant(idx)), EmptyTree, rhs)
                }
                Match(args(0).annotated(New(ref(defn.UncheckedAnnot.typeRef))), cases)
              }
            )

        val quoteClass = if isType then defn.QuotedTypeClass else defn.QuotedExprClass
        val quotedType = quoteClass.typeRef.appliedTo(originalTp)
        val lambdaTpe = MethodType(defn.QuotesClass.typeRef :: Nil, quotedType)
        def callUnpickle(ts: List[Tree]) = {
          val qctx = ts.head.asInstance(defn.QuoteUnpicklerClass.typeRef)
          val unpickleMeth = if isType then defn.QuoteUnpickler_unpickleType else defn.QuoteUnpickler_unpickleExpr
          qctx.select(unpickleMeth).appliedToType(originalTp).appliedTo(pickledQuoteStrings, typeHoles, termHoles)
        }
        Lambda(lambdaTpe, callUnpickle).withSpan(body.span)
      }

      /** Encode quote using Reflection.TypeRepr.typeConstructorOf
       *
       *  Generate the code
       *  ```scala
       *    qctx.reflect.TypeReprMethods.asType(
       *      qctx.reflect.TypeRepr.typeConstructorOf(classOf[<type>]])
       *    ).asInstanceOf[scala.quoted.Type[<type>]]
       *  ```
       *  this closure is always applied directly to the actual context and the BetaReduce phase removes it.
       */
      def taggedType() =
        val typeType = defn.QuotedTypeClass.typeRef.appliedTo(body.tpe)
        val classTree = TypeApply(ref(defn.Predef_classOf.termRef), body :: Nil)
        val reflect = quote.args.head.select("reflect".toTermName)
        val typeRepr = reflect.select("TypeRepr".toTermName).select("typeConstructorOf".toTermName).appliedTo(classTree)
        reflect.select("TypeReprMethods".toTermName).select("asType".toTermName).appliedTo(typeRepr).asInstance(typeType)

      if (isType) {
        if (splices.isEmpty && body.symbol.isPrimitiveValueClass) taggedType()
        else pickleAsTasty().select(nme.apply).appliedTo(quote.args.head) // TODO do not create lambda
      }
      else getLiteral(body) match {
        case Some(lit) => pickleAsValue(lit)
        case _ => pickleAsTasty()
      }
    }

    /** If inside a quote, split the body of the splice into a core and a list of embedded quotes
     *  and make a hole from these parts. Otherwise issue an error, unless we
     *  are in the body of an inline method.
     */
    protected def transformSplice(body: Tree, splice: Apply)(using Context): Tree =
      if (level > 1) {
        val body1 = nested(isQuote = false).transform(body)(using spliceContext)
        cpy.Apply(splice)(splice.fun, body1 :: Nil)
      }
      else {
        assert(level == 1, "unexpected top splice outside quote")
        val (body1, quotes) = nested(isQuote = false).splitSplice(body)(using spliceContext)
        val tpe = outer.nn.embedded.getHoleType(body, splice)
        val hole = makeHole(splice.isTerm, body1, quotes, tpe).withSpan(splice.span)
        // We do not place add the inline marker for trees that where lifted as they come from the same file as their
        // enclosing quote. Any intemediate splice will add it's own Inlined node and cancel it before splicig the lifted tree.
        // Note that lifted trees are not necessarily expressions and that Inlined nodes are expected to be expressions.
        // For example we can have a lifted tree containing the LHS of an assignment (see tests/run-with-compiler/quote-var.scala).
        if (outer.nn.embedded.isLiftedSymbol(body.symbol)) hole
        else Inlined(EmptyTree, Nil, hole).withSpan(splice.span)
      }

     /** If inside a quote, split the body of the splice into a core and a list of embedded quotes
     *  and make a hole from these parts. Otherwise issue an error, unless we
     *  are in the body of an inline method.
     */
     protected def transformSpliceType(body: Tree, splice: Select)(using Context): Tree =
      if level > 1 then
        val body1 = nested(isQuote = false).transform(body)(using spliceContext)
        cpy.Select(splice)(body1, splice.name)
      else if level == 1 then
        val (body1, quotes) = nested(isQuote = false).splitSplice(body)(using spliceContext)
        val tpe = outer.nn.embedded.getHoleType(body, splice)
        makeHole(splice.isTerm, body1, quotes, tpe).withSpan(splice.span)
      else
        splice

    /** Transforms the contents of a nested splice
     *  Assuming
     *     '{
     *        val x = ???
     *        val y = ???
     *        ${ ... '{ ... x .. y ... } ... }
     *      }
     *  then the spliced subexpression
     *     { ... '{ ... x ... y ... } ... }
     *  will be transformed to
     *     (args: Seq[Any]) => {
     *       val x$1 = args(0).asInstanceOf[Expr[Any]] // or .asInstanceOf[Type[Any]]
     *       val y$1 = args(1).asInstanceOf[Expr[Any]] // or .asInstanceOf[Type[Any]]
     *       { ... '{ ... ${x$1} ... ${y$1} ... } ... }
     *     }
     *
     *  See: `capture`
     *
     *  At the same time register embedded trees `x` and `y` to place as arguments of the hole
     *  placed in the original code.
     *     '{
     *        val x = ???
     *        val y = ???
     *        Hole(0 | x, y)
     *      }
     */
    private def makeLambda(tree: Tree)(using Context): Tree = {
      def body(arg: Tree)(using Context): Tree = {
        var i = 0
        transformWithCapturer(tree)(
          (captured: mutable.Map[Symbol, Tree]) => {
            (tree: Tree) => {
              def newCapture = {
                val tpw = tree.tpe.widen match {
                  case tpw: MethodicType => tpw.toFunctionType(isJava = false)
                  case tpw => tpw
                }
                assert(tpw.isInstanceOf[ValueType])
                val argTpe =
                  if (tree.isType) defn.QuotedTypeClass.typeRef.appliedTo(tpw)
                  else defn.FunctionType(1, isContextual = true).appliedTo(defn.QuotesClass.typeRef, defn.QuotedExprClass.typeRef.appliedTo(tpw))
                val selectArg = arg.select(nme.apply).appliedTo(Literal(Constant(i))).cast(argTpe)
                val capturedArg = SyntheticValDef(UniqueName.fresh(tree.symbol.name.toTermName).toTermName, selectArg)
                i += 1
                embedded.addTree(tree, capturedArg.symbol)
                captured.put(tree.symbol, capturedArg)
                capturedArg
              }
              val refSym = captured.getOrElseUpdate(tree.symbol, newCapture).symbol
              ref(refSym).withSpan(tree.span)
            }
          }
        )
      }
      /* Lambdas are generated outside the quote that is being reified (i.e. in outer.owner).
       * In case the case that level == -1 the code is not in a quote, it is in an inline method,
       * hence we should take that as owner directly.
       */
      val lambdaOwner = if (level == -1) ctx.owner else outer.nn.owner

      val tpe = MethodType(defn.SeqType.appliedTo(defn.AnyType) :: Nil, tree.tpe.widen)
      val meth = newSymbol(lambdaOwner, UniqueName.fresh(nme.ANON_FUN), Synthetic | Method, tpe)
      Closure(meth, tss => body(tss.head.head)(using ctx.withOwner(meth)).changeNonLocalOwners(meth)).withSpan(tree.span)
    }

    private def transformWithCapturer(tree: Tree)(capturer: mutable.Map[Symbol, Tree] => Tree => Tree)(using Context): Tree = {
      val captured = mutable.LinkedHashMap.empty[Symbol, Tree]
      val captured2 = capturer(captured)

      outer.nn.localSymbols.foreach(sym => if (!sym.isInlineMethod) capturers.put(sym, captured2))

      val tree2 = transform(tree)
      capturers --= outer.nn.localSymbols

      val captures = captured.result().valuesIterator.toList
      if (captures.isEmpty) tree2
      else Block(captures, tree2)
    }

    /** Returns true if this tree will be captured by `makeLambda`. Checks phase consistency and presence of capturer. */
    private def isCaptured(sym: Symbol, level: Int)(using Context): Boolean =
      level == 1 && levelOf(sym) == 1 && capturers.contains(sym)

    /** Transform `tree` and return the resulting tree and all `embedded` quotes
     *  or splices as a pair.
     */
    private def splitQuote(tree: Tree)(using Context): (Tree, List[Tree]) = {
      val tree1 = stipTypeAnnotations(transform(tree))
      (tree1, embedded.getTrees)
    }

    private def splitSplice(tree: Tree)(using Context): (Tree, List[Tree]) = {
      val tree1 = makeLambda(tree)
      (tree1, embedded.getTrees)
    }

    private def stipTypeAnnotations(tree: Tree)(using Context): Tree =
      new TreeTypeMap(typeMap = _.stripAnnots).apply(tree)

    /** Register `body` as an `embedded` quote or splice
     *  and return a hole with `splices` as arguments and the given type `tpe`.
     */
    private def makeHole(isTermHole: Boolean, body: Tree, splices: List[Tree], tpe: Type)(using Context): Hole = {
      val idx = embedded.addTree(body, NoSymbol)

      /** Remove references to local types that will not be defined in this quote */
      def getTypeHoleType(using Context) = new TypeMap() {
        override def apply(tp: Type): Type = tp match
          case tp: TypeRef if tp.typeSymbol.isTypeSplice =>
            apply(tp.dealias)
          case tp @ TypeRef(pre, _) if pre == NoPrefix || pre.termSymbol.isLocal =>
            val hiBound = tp.typeSymbol.info match
              case info: ClassInfo => info.parents.reduce(_ & _)
              case info => info.hiBound
            apply(hiBound)
          case tp =>
            mapOver(tp)
      }

      /** Remove references to local types that will not be defined in this quote */
      def getTermHoleType(using Context) = new TypeMap() {
        override def apply(tp: Type): Type = tp match
          case tp @ TypeRef(NoPrefix, _) if capturers.contains(tp.symbol) =>
            // reference to term with a type defined in outer quote
            getTypeHoleType(tp)
          case tp @ TermRef(NoPrefix, _) if capturers.contains(tp.symbol) =>
            // widen term refs to terms defined in outer quote
            apply(tp.widenTermRefExpr)
          case tp =>
            mapOver(tp)
      }

      val holeType = if isTermHole then getTermHoleType(tpe) else getTypeHoleType(tpe)

      Hole(isTermHole, idx, splices).withType(holeType).asInstanceOf[Hole]
    }

    override def transform(tree: Tree)(using Context): Tree =
      if (tree.source != ctx.source && tree.source.exists)
        transform(tree)(using ctx.withSource(tree.source))
      else reporting.trace(i"Reifier.transform $tree at $level", show = true) {
        tree match {
          case Apply(TypeApply(fn, (body: RefTree) :: Nil), _)
          if fn.symbol == defn.QuotedTypeModule_of && isCaptured(body.symbol, level + 1) =>
            // Optimization: avoid the full conversion when capturing `X` with `x$1: Type[X$1]`
            // in `Type.of[X]` to `Type.of[x$1.Underlying]` and go directly to `X$1`
            capturers(body.symbol)(body)
          case Apply(Select(Apply(TypeApply(fn,_), List(ref: RefTree)),nme.apply),List(quotes))
          if fn.symbol == defn.QuotedRuntime_exprQuote && isCaptured(ref.symbol, level + 1) =>
            // Optimization: avoid the full conversion when capturing `x` with `x$1: Expr[X]`
            // in `'{x}` to `'{ ${x$1} }'` and go directly to `x$1`
            capturers(ref.symbol)(ref).select(nme.apply).appliedTo(quotes)
          case tree: RefTree if isCaptured(tree.symbol, level) =>
            val body = capturers(tree.symbol).apply(tree)
            if (tree.isType)
              transformSpliceType(body, body.select(tpnme.Underlying))
            else
              val splice = ref(defn.QuotedRuntime_exprSplice).appliedToType(tree.tpe).appliedTo(body)
              transformSplice(body, splice)

          case tree: DefDef if tree.symbol.is(Macro) && level == 0 =>
            // Shrink size of the tree. The methods have already been inlined.
            // TODO move to FirstTransform to trigger even without quotes
            cpy.DefDef(tree)(rhs = defaultValue(tree.rhs.tpe))

          case tree: DefTree if level >= 1 =>
            val newAnnotations = tree.symbol.annotations.mapconserve { annot =>
              val newAnnotTree = transform(annot.tree)(using ctx.withOwner(tree.symbol))
              if (annot.tree == newAnnotTree) annot
              else ConcreteAnnotation(newAnnotTree)
            }
            tree.symbol.annotations = newAnnotations
            super.transform(tree)
          case _ =>
            super.transform(tree)
        }
      }

    private def liftList(list: List[Tree], tpe: Type)(using Context): Tree =
      list.foldRight[Tree](ref(defn.NilModule)) { (x, acc) =>
        acc.select("::".toTermName).appliedToType(tpe).appliedTo(x)
      }
  }
}


object PickleQuotes {
  import tpd._

  val name: String = "pickleQuotes"
  val description: String = "turn quoted trees into explicit run-time data structures"

  def getLiteral(tree: tpd.Tree): Option[Literal] = tree match {
    case tree: Literal => Some(tree)
    case Block(Nil, e) => getLiteral(e)
    case Inlined(_, Nil, e) => getLiteral(e)
    case _ => None
  }

  class Embedded(trees: mutable.ListBuffer[tpd.Tree] = mutable.ListBuffer.empty, map: mutable.Map[Symbol, tpd.Tree] = mutable.Map.empty) {
    /** Adds the tree and returns it's index */
    def addTree(tree: tpd.Tree, liftedSym: Symbol): Int = {
      trees += tree
      if (liftedSym ne NoSymbol)
        map.put(liftedSym, tree)
      trees.length - 1
    }

    /** Type used for the hole that will replace this splice */
    def getHoleType(body: tpd.Tree, splice: tpd.Tree)(using Context): Type =
      // For most expressions the splice.tpe but there are some types that are lost by lifting
      // that can be recoverd from the original tree. Currently the cases are:
      //  * Method types: the splice represents a method reference
      map.get(body.symbol).map(_.tpe.widen).getOrElse(splice.tpe)

    def isLiftedSymbol(sym: Symbol)(using Context): Boolean = map.contains(sym)

    /** Get the list of embedded trees */
    def getTrees: List[tpd.Tree] = trees.toList

    override def toString: String = s"Embedded($trees, $map)"
  }
}
