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
import ast.{TreeTypeMap, untpd}
import util.Spans._
import util.SourcePosition
import tasty.TreePickler.Hole
import SymUtils._
import NameKinds._
import dotty.tools.dotc.ast.tpd
import typer.Implicits.SearchFailureType

import scala.collection.mutable
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.quoted._
import dotty.tools.dotc.transform.TreeMapWithStages._
import dotty.tools.dotc.typer.Inliner
import dotty.tools.dotc.util.SourcePosition

import scala.annotation.constructorOnly


/** Translates quoted terms and types to `unpickle` method calls.
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
 *     unpickle(
 *       [[ // PICKLED TASTY
 *         ...
 *         val x1 = ???
 *         val x2 = ???
 *         ...
 *         Hole(0 | x1, x2)
 *         ...
 *       ]],
 *       List(
 *         (args: Seq[Any]) => {
 *           val x1$1 = args(0).asInstanceOf[Expr[T]]
 *           val x2$1 = args(1).asInstanceOf[Expr[T]] // can be asInstanceOf[Type[T]]
 *           ...
 *           { ... '{ ... ${x1$1} ... ${x2$1} ...} ... }
 *         }
 *       )
 *     )
 *    ```
 *  and then performs the same transformation on `'{ ... ${x1$1} ... ${x2$1} ...}`.
 *
 */
class ReifyQuotes extends MacroTransform {
  import ReifyQuotes._
  import tpd._

  override def phaseName: String = ReifyQuotes.name

  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = {
    tree match {
      case tree: RefTree if !ctx.inInlineMethod =>
        assert(!tree.symbol.isQuote)
        assert(!tree.symbol.isSplice)
      case _ =>
    }
  }

  override def run(implicit ctx: Context): Unit =
    if (ctx.compilationUnit.needsStaging) super.run(freshStagingContext)

  protected def newTransformer(implicit ctx: Context): Transformer = new Transformer {
    override def transform(tree: tpd.Tree)(implicit ctx: Context): tpd.Tree =
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
  private class QuoteReifier(outer: QuoteReifier, capturers: mutable.HashMap[Symbol, Tree => Tree],
                             val embedded: Embedded, val owner: Symbol)(@constructorOnly ictx: Context) extends TreeMapWithStages(ictx) { self =>

    import StagingContext._

    /** A nested reifier for a quote (if `isQuote = true`) or a splice (if not) */
    def nested(isQuote: Boolean)(implicit ctx: Context): QuoteReifier = {
      val nestedEmbedded = if (level > 1 || (level == 1 && isQuote)) embedded else new Embedded
      new QuoteReifier(this, capturers, nestedEmbedded, ctx.owner)(ctx)
    }

    /** Assuming <expr> contains types `${<tag1>}, ..., ${<tagN>}`, the expression
     *
     *      { type <Type1> = ${<tag1>}
     *        ...
     *        type <TypeN> = ${<tagN>}
     *        <expr>
     *      }
     *
     *  references to `TypeI` in `expr` are rewired to point to the locally
     *  defined versions. As a side effect, prepend the expressions `tag1, ..., `tagN`
     *  as splices.
     */
    private def addTags(expr: Tree)(implicit ctx: Context): Tree = {

      def mkTagSymbolAndAssignType(spliced: TermRef): TypeDef = {
        val splicedTree = tpd.ref(spliced)
        val rhs = transform(splicedTree.select(tpnme.splice))
        val alias = ctx.typeAssigner.assignType(untpd.TypeBoundsTree(rhs, rhs), rhs, rhs)
        val local = ctx.newSymbol(
          owner = ctx.owner,
          name = UniqueName.fresh((splicedTree.symbol.name.toString + "$_").toTermName).toTypeName,
          flags = Synthetic,
          info = TypeAlias(splicedTree.tpe.select(tpnme.splice)),
          coord = spliced.termSymbol.coord).asType

        ctx.typeAssigner.assignType(untpd.TypeDef(local.name, alias), local)
      }

      val tagDefCache = new mutable.LinkedHashMap[Symbol, TypeDef]()

      def typeTagMap = new TypeMap() {
        def apply(tp: Type): Type = tp match {
          case tp: TypeRef if tp.symbol.isSplice =>
            tp.prefix match {
              case prefix: TermRef =>
                val tagDef = tagDefCache.getOrElseUpdate(prefix.symbol, mkTagSymbolAndAssignType(prefix))
                tagDef.symbol.typeRef
            }
          case _ =>
            mapOver(tp)
        }
      }

      val tagedTree = new TreeTypeMap(typeMap = typeTagMap).apply(expr)

      if (tagDefCache.isEmpty) expr
      else Block(tagDefCache.valuesIterator.toList, tagedTree)
    }

    /** Split `body` into a core and a list of embedded splices.
     *  Then if inside a splice, make a hole from these parts.
     *  If outside a splice, generate a call tp `scala.quoted.Unpickler.unpickleType` or
     *  `scala.quoted.Unpickler.unpickleExpr` that matches `tpe` with
     *  core and splices as arguments.
     */
    override protected def transformQuotation(body: Tree, quote: Tree)(implicit ctx: Context): Tree = {
      val isType = quote.symbol eq defn.InternalQuoted_typeQuote
      assert(!(body.symbol.isSplice && (body.isInstanceOf[GenericApply[_]] || body.isInstanceOf[Select])))
      if (level > 0) {
        val body1 = nested(isQuote = true).transform(body)(quoteContext)
        super.transformQuotation(body1, quote)
      }
      else body match {
        case body: RefTree if isCaptured(body.symbol, level + 1) =>
          // Optimization: avoid the full conversion when capturing `x`
          // in '{ x } to '{ ${x$1} } and go directly to `x$1`
          capturers(body.symbol)(body)
        case _=>
          val (body1, splices) = nested(isQuote = true).splitQuote(body)(quoteContext)
          if (level == 0) {
            val body2 =
              if (body1.isType) body1
              else Inlined(Inliner.inlineCallTrace(ctx.owner, quote.sourcePos), Nil, body1)
            pickledQuote(body2, splices, body.tpe, isType).withSpan(quote.span)
          }
          else {
            body
          }
      }
    }

    private def pickledQuote(body: Tree, splices: List[Tree], originalTp: Type, isType: Boolean)(implicit ctx: Context) = {
      def pickleAsValue[T](value: T) =
        ref(defn.Unpickler_liftedExpr).appliedToType(originalTp.widen).appliedTo(Literal(Constant(value)))
      def pickleAsTasty() = {
        val meth =
          if (isType) ref(defn.Unpickler_unpickleType).appliedToType(originalTp)
          else ref(defn.Unpickler_unpickleExpr).appliedToType(originalTp.widen)
        meth.appliedTo(
          liftList(PickledQuotes.pickleQuote(body).map(x => Literal(Constant(x))), defn.StringType),
          liftList(splices, defn.AnyType))
      }
      if (splices.nonEmpty) pickleAsTasty()
      else if (isType) {
        def tag(tagName: String) = ref(defn.QuotedTypeModule).select(tagName.toTermName)
        if (body.symbol.isPrimitiveValueClass) tag(s"${body.symbol.name}Tag")
        else pickleAsTasty()
      }
      else toValue(body) match {
        case Some(value) => pickleAsValue(value)
        case _ => pickleAsTasty()
      }
    }

    /** If inside a quote, split the body of the splice into a core and a list of embedded quotes
     *  and make a hole from these parts. Otherwise issue an error, unless we
     *  are in the body of an inline method.
     */
    protected def transformSplice(body: Tree, splice: Tree)(implicit ctx: Context): Tree = {
      if (level > 1) {
        val body1 = nested(isQuote = false).transform(body)(spliceContext)
        splice match {
          case splice: Apply => cpy.Apply(splice)(splice.fun, body1 :: Nil)
          case splice: Select => cpy.Select(splice)(body1, splice.name)
        }
      }
      else {
        assert(level == 1, "unexpected top splice outside quote")
        val (body1, quotes) = nested(isQuote = false).splitSplice(body)(spliceContext)
        val tpe = outer.embedded.getHoleType(body, splice)
        val hole = makeHole(body1, quotes, tpe).withSpan(splice.span)
        // We do not place add the inline marker for trees that where lifted as they come from the same file as their
        // enclosing quote. Any intemediate splice will add it's own Inlined node and cancel it before splicig the lifted tree.
        // Note that lifted trees are not necessarily expressions and that Inlined nodes are expected to be expressions.
        // For example we can have a lifted tree containing the LHS of an assignment (see tests/run-with-compiler/quote-var.scala).
        if (splice.isType || outer.embedded.isLiftedSymbol(body.symbol)) hole
        else Inlined(EmptyTree, Nil, hole).withSpan(splice.span)
      }
    }

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
    private def makeLambda(tree: Tree)(implicit ctx: Context): Tree = {
      def body(arg: Tree)(implicit ctx: Context): Tree = {
        var i = 0
        transformWithCapturer(tree)(
          (captured: mutable.Map[Symbol, Tree]) => {
            (tree: Tree) => {
              def newCapture = {
                val tpw = tree.tpe.widen match {
                  case tpw: MethodicType => tpw.toFunctionType()
                  case tpw => tpw
                }
                assert(tpw.isInstanceOf[ValueType])
                val argTpe =
                  if (tree.isType) defn.QuotedTypeType.appliedTo(tpw)
                  else defn.QuotedExprType.appliedTo(tpw)
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
      /* Lambdas are generated outside the quote that is beeing reified (i.e. in outer.owner).
       * In case the case that level == -1 the code is not in a quote, it is in an inline method,
       * hence we should take that as owner directly.
       */
      val lambdaOwner = if (level == -1) ctx.owner else outer.owner

      val tpe = MethodType(defn.SeqType.appliedTo(defn.AnyType) :: Nil, tree.tpe.widen)
      val meth = ctx.newSymbol(lambdaOwner, UniqueName.fresh(nme.ANON_FUN), Synthetic | Method, tpe)
      val closure = Closure(meth, tss => body(tss.head.head)(ctx.withOwner(meth)).changeOwner(ctx.owner, meth)).withSpan(tree.span)

      enclosingInlineds match {
        case enclosingInline :: _ =>
         // In case a tree was inlined inside of the quote and we this closure corresponds to code within it we need to keep the inlined node.
         Inlined(enclosingInline, Nil, closure)(ctx.withSource(lambdaOwner.topLevelClass.source))
        case Nil => closure
      }
    }

    private def transformWithCapturer(tree: Tree)(capturer: mutable.Map[Symbol, Tree] => Tree => Tree)(implicit ctx: Context): Tree = {
      val captured = mutable.LinkedHashMap.empty[Symbol, Tree]
      val captured2 = capturer(captured)

      outer.localSymbols.foreach(sym => if (!sym.isInlineMethod) capturers.put(sym, captured2))

      val tree2 = transform(tree)
      capturers --= outer.localSymbols

      seq(captured.result().valuesIterator.toList, tree2)
    }

    /** Returns true if this tree will be captured by `makeLambda`. Checks phase consistency and presence of capturer. */
    private def isCaptured(sym: Symbol, level: Int)(implicit ctx: Context): Boolean =
      level == 1 && levelOf(sym).contains(1) && capturers.contains(sym)

    /** Transform `tree` and return the resulting tree and all `embedded` quotes
     *  or splices as a pair, after performing the `addTags` transform.
     */
    private def splitQuote(tree: Tree)(implicit ctx: Context): (Tree, List[Tree]) = {
      val tree1 = addTags(transform(tree))
      (tree1, embedded.getTrees)
    }

    private def splitSplice(tree: Tree)(implicit ctx: Context): (Tree, List[Tree]) = {
      val tree1 = makeLambda(tree)
      (tree1, embedded.getTrees)
    }

    /** Register `body` as an `embedded` quote or splice
     *  and return a hole with `splices` as arguments and the given type `tpe`.
     */
    private def makeHole(body: Tree, splices: List[Tree], tpe: Type)(implicit ctx: Context): Hole = {
      val idx = embedded.addTree(body, NoSymbol)
      Hole(idx, splices).withType(tpe).asInstanceOf[Hole]
    }

    override def transform(tree: Tree)(implicit ctx: Context): Tree = {
      if (tree.source != ctx.source && tree.source.exists)
        transform(tree)(ctx.withSource(tree.source))
      else reporting.trace(i"Reifier.transform $tree at $level", show = true) {
        tree match {
          case tree: RefTree if isCaptured(tree.symbol, level) =>
            val body = capturers(tree.symbol).apply(tree)
            val splice: Tree =
              if (tree.isType) body.select(tpnme.splice)
              else ref(defn.InternalQuoted_exprSplice).appliedToType(tree.tpe).appliedTo(body)

            transformSplice(body, splice)

          case tree: DefDef if tree.symbol.is(Macro) && level == 0 =>
            // Shrink size of the tree. The methods have already been inlined.
            // TODO move to FirstTransform to trigger even without quotes
            cpy.DefDef(tree)(rhs = defaultValue(tree.rhs.tpe))

          case _ =>
            super.transform(tree)
        }
      }
    }

    private def liftList(list: List[Tree], tpe: Type)(implicit ctx: Context): Tree = {
      list.foldRight[Tree](ref(defn.NilModule)) { (x, acc) =>
        acc.select("::".toTermName).appliedToType(tpe).appliedTo(x)
      }
    }
  }
}


object ReifyQuotes {
  import tpd._

  val name: String = "reifyQuotes"

  def toValue(tree: tpd.Tree): Option[Any] = tree match {
    case Literal(Constant(c)) => Some(c)
    case Block(Nil, e) => toValue(e)
    case Inlined(_, Nil, e) => toValue(e)
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
    def getHoleType(body: tpd.Tree, splice: tpd.Tree)(implicit ctx: Context): Type = {
      // For most expressions the splice.tpe but there are some types that are lost by lifting
      // that can be recoverd from the original tree. Currently the cases are:
      //  * Method types: the splice represents a method reference
      map.get(body.symbol).map(_.tpe.widen).getOrElse(splice.tpe)
    }

    def isLiftedSymbol(sym: Symbol)(implicit ctx: Context): Boolean = map.contains(sym)

    /** Get the list of embedded trees */
    def getTrees: List[tpd.Tree] = trees.toList

    override def toString: String = s"Embedded($trees, $map)"

  }
}
