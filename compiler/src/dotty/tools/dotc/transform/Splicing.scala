package dotty.tools
package dotc
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
import SymUtils._
import NameKinds._
import dotty.tools.dotc.ast.tpd
import StagingContext._

import scala.collection.mutable
import dotty.tools.dotc.core.Annotations._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.quoted._
import dotty.tools.dotc.transform.TreeMapWithStages._
import dotty.tools.dotc.typer.Inliner
import dotty.tools.dotc.config.ScalaRelease.*

import scala.annotation.constructorOnly

object Splicing:
  val name: String = "splicing"

/** Transforms level 1 splices into holes. To do so it transforms the contents of the splice into
 *  a lambda that receives all cross-quote references.
 *
 *  Cross-quote reference is a reference to a definition that is not defined in the current quote.
 *  Those references appear in quotes that are nested in a splice.
 *
 *  After this phase we have the invariant where all splices have the following shape
 *  ```
 *  {{{ <holeIdx> | <holeType> | <captures>* | (<capturedTerms>*) => <spliceContent> }}}
 *  ```
 *  where `<spliceContent>` does not contain any free references to quoted definitions and `<captures>*`
 *  contains the quotes with references to all cross-quote references. There are some special rules
 *  for references in the LHS of assignments and cross-quote method references.
 *
 *  In the following code example `x1` and `x2` are cross-quote references.
 *  ```
 *  '{ ...
 *    val x1: T1 = ???
 *    val x2: T2 = ???
 *    ${ (q: Quotes) ?=> f('{ g(x1, x2) }) }: T3
 *  }
 *  ```
 *
 *  This phase identifies cross-quote references such as `x1` and replaces it with an `${x1$}`.
 *  All cross-quote arguments are directly applied in the lambda.
 *
 *  ```
 *  '{ ...
 *     val x1: T1 = ???
 *     val x2: T2 = ???
 *     {{{ 0 | T3 | x1, x2 |
 *       (x1$: Expr[T1], x2$: Expr[T2]) => // body of this lambda does not contain references to x1 or x2
 *         (q: Quotes) ?=> f('{ g(${x1$}, ${x2$}) })
 *
 *     }}}
 *   }
 *   ```
 *
 *  and then performs the same transformation on `'{ g(${x1$}, ${x2$}) }`.
 *
 */
class Splicing extends MacroTransform:
  import tpd._

  override def phaseName: String = Splicing.name

  override def run(using Context): Unit =
    if ctx.compilationUnit.needsStaging then
      super.run(using freshStagingContext)

  protected def newTransformer(using Context): Transformer = Level0QuoteTransformer

  /** Transforms all quotes at level 0 using the `QuoteTransformer` */
  private object Level0QuoteTransformer extends Transformer:
    override def transform(tree: tpd.Tree)(using Context): tpd.Tree =
      assert(level == 0)
      tree match
        case Apply(Select(Apply(TypeApply(fn,_), List(code)),nme.apply),List(quotes))
        if fn.symbol == defn.QuotedRuntime_exprQuote =>
          QuoteTransformer().transform(tree)
        case TypeApply(_, _) if tree.symbol == defn.QuotedTypeModule_of =>
          QuoteTransformer().transform(tree)
        case tree: DefDef if tree.symbol.is(Inline) =>
          // Quotes in inlined methods are only pickled after they are inlined.
          tree
        case _ =>
          super.transform(tree)
  end Level0QuoteTransformer


  /** Transforms all direct splices in the current quote and replace them with holes. */
  private class QuoteTransformer() extends Transformer:
    /** Set of definitions in the current quote */
    private val quotedDefs = mutable.Set.empty[Symbol]

    /** Number of holes created in this quote. Used for indexing holes. */
    private var numHoles = 0

    /** Mapping from the term symbol of a `Type[T]` to it's hole. Used to deduplicate type holes. */
    private val typeHoles = mutable.Map.empty[Symbol, Hole]

    override def transform(tree: tpd.Tree)(using Context): tpd.Tree =
      tree match
        case Apply(fn, List(splicedCode)) if fn.symbol == defn.QuotedRuntime_exprNestedSplice =>
          if level > 1 then
            val splicedCode1 = super.transform(splicedCode)(using spliceContext)
            cpy.Apply(tree)(fn, List(splicedCode1))
          else
            val holeIdx = numHoles
            numHoles += 1
            val splicer = SpliceTransformer(ctx.owner, quotedDefs.contains)
            val newSplicedCode1 = splicer.transformSplice(splicedCode, tree.tpe, holeIdx)(using spliceContext)
            val newSplicedCode2 = Level0QuoteTransformer.transform(newSplicedCode1)(using spliceContext)
            newSplicedCode2
        case tree: TypeDef if tree.symbol.hasAnnotation(defn.QuotedRuntime_SplicedTypeAnnot) =>
          val tp @ TypeRef(qual: TermRef, _) = tree.rhs.tpe.hiBound
          quotedDefs += tree.symbol
          val hole = typeHoles.get(qual.symbol) match
            case Some (hole) => cpy.Hole(hole)(content = EmptyTree)
            case None =>
              val holeIdx = numHoles
              numHoles += 1
              val hole = tpd.Hole(false, holeIdx, Nil, ref(qual), TypeTree(tp))
              typeHoles.put(qual.symbol, hole)
              hole
          cpy.TypeDef(tree)(rhs = hole)
        case Apply(Select(Apply(TypeApply(fn,_), List(code)),nme.apply),List(quotes))
        if fn.symbol == defn.QuotedRuntime_exprQuote =>
          super.transform(tree)(using quoteContext)
        case _: Template =>
          for sym <- tree.symbol.owner.info.decls do
            quotedDefs += sym
          super.transform(tree)
        case tree: DefTree =>
          quotedDefs += tree.symbol
          transformAnnotations(tree)
          super.transform(tree)
        case _: TypeTree =>
          super.transform(tree).withType(transformAnnotTrees(tree.tpe))
        case _ =>
          super.transform(tree)

    private def transformAnnotations(tree: DefTree)(using Context): Unit =
      tree.symbol.annotations = tree.symbol.annotations.mapconserve { annot =>
        val newAnnotTree = transform(annot.tree)(using ctx.withOwner(tree.symbol))
        if (annot.tree == newAnnotTree) annot
        else ConcreteAnnotation(newAnnotTree)
      }

    /** Transform trees within annotations */
    private def transformAnnotTrees(using Context) = new TypeMap {
      override def apply(tp: Type): Type = {
          tp match
            case tp @ AnnotatedType(underlying, annot) =>
              val underlying1 = this(underlying)
              derivedAnnotatedType(tp, underlying1, annot.derivedAnnotation(transform(annot.tree)))
            case _ => mapOver(tp)
      }
    }

  end QuoteTransformer

  /** Transforms a splice at level 1 into a hole
   *
   *  Finds all terms and types that are defined in the current quote and used within this splice.
   *  The resulting hole will contain all of these terms and types as arguments.
   *  Note that these captured variables are stage correct.
   *
   *  For a `x` of type `T1` and a type `X` defined in the current quote
   *  ```scala
   *  ${  (using Quotes) ?=> {... x ... X ...} }: T2
   *  ```
   *  is transformed into
   * ```scala
   *  {{{ <holeIdx++> | T2 | x, X | (x$1: Expr[T1], X$1: Type[X]) => (using Quotes) ?=> {... ${x$1} ...  X$1.Underlying ...}  }}}
   *  ```
   */
  private class SpliceTransformer(spliceOwner: Symbol, isCaptured: Symbol => Boolean) extends Transformer:
    private var refBindingMap = mutable.Map.empty[Symbol, (Tree, Symbol)]
    /** Reference to the `Quotes` instance of the current level 1 splice */
    private var quotes: Tree | Uninitialized = initiallyNull // TODO: add to the context
    private var healedTypes: PCPCheckAndHeal.QuoteTypeTags | Uninitialized = initiallyNull // TODO: add to the context

    def transformSplice(tree: tpd.Tree, tpe: Type, holeIdx: Int)(using Context): tpd.Tree =
      assert(level == 0)
      val newTree = transform(tree)
      val (refs, bindings) = refBindingMap.values.toList.unzip
      val bindingsTypes = bindings.map(_.termRef.widenTermRefExpr)
      val methType = MethodType(bindingsTypes, newTree.tpe)
      val meth = newSymbol(spliceOwner, nme.ANON_FUN, Synthetic | Method, methType)
      val ddef = DefDef(meth, List(bindings), newTree.tpe, newTree.changeOwner(ctx.owner, meth))
      val fnType = defn.FunctionType(bindings.size, isContextual = false).appliedTo(bindingsTypes :+ newTree.tpe)
      val closure = Block(ddef :: Nil, Closure(Nil, ref(meth), TypeTree(fnType)))
      tpd.Hole(true, holeIdx, refs, closure, TypeTree(tpe))

    override def transform(tree: tpd.Tree)(using Context): tpd.Tree =
      tree match
        case tree: RefTree =>
          if tree.isTerm then
            if isCaptured(tree.symbol) then
              val tpe = tree.tpe.widenTermRefExpr match {
                case tpw: MethodicType => tpw.toFunctionType(isJava = false)
                case tpw => tpw
              }
              spliced(tpe)(capturedTerm(tree))
            else super.transform(tree)
          else // tree.isType then
            if containsCapturedType(tree.tpe) then
              if level >= 1 then getTagRefFor(tree)
              else
                // Dealias references to captured types
                TypeTree(tree.tpe.dealias)
            else super.transform(tree)
        case tree: TypeTree =>
          if containsCapturedType(tree.tpe) && level >= 1 then getTagRefFor(tree)
          else tree
        case tree @ Assign(lhs: RefTree, rhs) =>
          if isCaptured(lhs.symbol) then transformSplicedAssign(tree)
          else super.transform(tree)
        case Apply(fn, args) if fn.symbol == defn.QuotedRuntime_exprNestedSplice =>
          val newArgs = args.mapConserve(arg => transform(arg)(using spliceContext))
          cpy.Apply(tree)(fn, newArgs)
        case Apply(sel @ Select(app @ Apply(fn, args),nme.apply), quotesArgs)
        if fn.symbol == defn.QuotedRuntime_exprQuote =>
          args match
            case List(tree: RefTree) if isCaptured(tree.symbol) =>
              capturedTerm(tree)
            case _ =>
              val newArgs = withCurrentQuote(quotesArgs.head) {
                if level > 1 then args.mapConserve(arg => transform(arg)(using quoteContext))
                else args.mapConserve(arg => transformLevel0QuoteContent(arg)(using quoteContext))
              }
              cpy.Apply(tree)(cpy.Select(sel)(cpy.Apply(app)(fn, newArgs), nme.apply), quotesArgs)
        case Apply(TypeApply(_, List(tpt)), List(quotes))
        if tree.symbol == defn.QuotedTypeModule_of && containsCapturedType(tpt.tpe) =>
          ref(capturedType(tpt))(using ctx.withSource(tree.source)).withSpan(tree.span)
        case CapturedApplication(fn, argss) =>
          transformCapturedApplication(tree, fn, argss)
        case _ =>
          super.transform(tree)

    private def transformLevel0QuoteContent(tree: Tree)(using Context): Tree =
      // transform and collect new healed types
      val old = healedTypes
      healedTypes = new PCPCheckAndHeal.QuoteTypeTags(tree.span)
      val tree1 = transform(tree)
      val newHealedTypes = healedTypes.nn.getTypeTags
      healedTypes = old
      // add new healed types to the current, merge with existing healed types if necessary
      if newHealedTypes.isEmpty then tree1
      else tree1 match
        case Block(stats @ (x :: _), expr) if x.symbol.hasAnnotation(defn.QuotedRuntime_SplicedTypeAnnot) =>
          Block(newHealedTypes ::: stats, expr)
        case _ =>
          Block(newHealedTypes, tree1)

    class ArgsClause(val args: List[Tree]):
      def isTerm: Boolean = args.isEmpty || args.head.isTerm

    private object CapturedApplication {

      /** Matches and application `f(...)` (possibly with several argument clauses) where `f` is captured */
      def unapply(tree: Tree)(using Context): Option[(RefTree, List[ArgsClause])] = tree match
        case GenericApply(fn: RefTree, args) if isCaptured(fn.symbol) =>
          Some((fn, ArgsClause(args) :: Nil))
        case GenericApply(CapturedApplication(fn, argss), args) =>
          Some((fn, argss :+ ArgsClause(args)))
        case _ =>
          None
    }

    private def containsCapturedType(tpe: Type)(using Context): Boolean =
      tpe.existsPart(t => isCaptured(t.typeSymbol) || isCaptured(t.termSymbol), StopAt.Static)

    /** Transform an assignment `x = e` with a captured `x` to
     * `${ Assign(x$1.asTerm, '{e}.asTerm).asExpr.asInstanceOf[Expr[T]] }`
     *
     *  Registers `x` as a captured variable in the hole and creates an `x$1` `Expr` reference to it.
     */
    private def transformSplicedAssign(tree: Assign)(using Context): Tree =
      spliced(tree.tpe) {
        reflect.asExpr(tree.tpe)(
          reflect.Assign(
            reflect.asTerm(capturedTerm(tree.lhs)),
            reflect.asTerm(quoted(tree.rhs))
          )
        )
      }

    /** Transform an application `f(a1, a2, ...)` with a captured `f` to
     * `${ Apply(f$1.asTerm, List('{a1$}.asTerm, '{a2$}.asTerm, ...)).asExpr.asInstanceOf[Expr[T]] }`
     *
     *  Registers `f` as a captured variable in the hole and creates an `f$1` `Expr` reference to it.
     *
     *  It also handles cases with multiple argument clauses using nested `Apply`/`TypeApply`.
     */
    private def transformCapturedApplication(tree: Tree, fn: RefTree, argss: List[ArgsClause])(using Context): Tree =
      spliced(tree.tpe) {
        def TermList(args: List[Tree]): List[Tree] =
          args.map(arg => reflect.asTerm(quoted(transform(arg)(using spliceContext))))
        def TypeTreeList(args: List[Tree]): List[Tree] =
          args.map(arg => reflect.Inferred(reflect.TypeReprOf(transform(arg)(using spliceContext).tpe)))
        reflect.asExpr(tree.tpe) {
          argss.foldLeft[Tree](reflect.asTerm(capturedTerm(fn, defn.AnyType))) { (acc, clause) =>
            if clause.isTerm then reflect.Apply(acc, TermList(clause.args))
            else reflect.TypeApply(acc, TypeTreeList(clause.args))
          }
        }
      }

    private def capturedTerm(tree: Tree)(using Context): Tree =
      val tpe = tree.tpe.widenTermRefExpr match
        case tpw: MethodicType => tpw.toFunctionType(isJava = false)
        case tpw => tpw
      capturedTerm(tree, tpe)

    private def capturedTerm(tree: Tree, tpe: Type)(using Context): Tree =
      def newBinding = newSymbol(
        spliceOwner,
        UniqueName.fresh(tree.symbol.name.toTermName).toTermName,
        Param,
        defn.QuotedExprClass.typeRef.appliedTo(tpe),
      )
      val bindingSym = refBindingMap.getOrElseUpdate(tree.symbol, (tree, newBinding))._2
      ref(bindingSym)

    private def capturedType(tree: Tree)(using Context): Symbol =
      val tpe = tree.tpe.widenTermRefExpr
      def newBinding = newSymbol(
        spliceOwner,
        UniqueName.fresh(nme.Type).toTermName,
        Param,
        defn.QuotedTypeClass.typeRef.appliedTo(tpe),
      )
      val bindingSym = refBindingMap.getOrElseUpdate(tree.symbol, (TypeTree(tree.tpe), newBinding))._2
      bindingSym

    private def getTagRefFor(tree: Tree)(using Context): Tree =
      val capturedTypeSym = capturedType(tree)
      TypeTree(healedTypes.nn.getTagRef(capturedTypeSym.termRef))

    private def withCurrentQuote[T](newQuotes: Tree)(body: => T)(using Context): T =
      if level == 0 then
        val savedQuotes = quotes
        quotes = newQuotes
        try body
        finally quotes = savedQuotes
      else body

    private def spliced(tpe: Type)(body: Context ?=> Tree)(using Context): Tree =
      val exprTpe = defn.QuotedExprClass.typeRef.appliedTo(tpe)
      val closure =
        val methTpe = ContextualMethodType(List(defn.QuotesClass.typeRef), exprTpe)
        val meth = newSymbol(ctx.owner, nme.ANON_FUN, Synthetic | Method, methTpe)
        Closure(meth, argss => {
          withCurrentQuote(argss.head.head) {
            body(using ctx.withOwner(meth)).changeOwner(ctx.owner, meth)
          }
        })
      ref(defn.QuotedRuntime_exprNestedSplice)
        .appliedToType(tpe)
        .appliedTo(Literal(Constant(null))) // Dropped when creating the Hole that contains it
        .appliedTo(closure)

    private def quoted(expr: Tree)(using Context): Tree =
      val tpe = expr.tpe.widenTermRefExpr
      ref(defn.QuotedRuntime_exprQuote)
        .appliedToType(tpe)
        .appliedTo(expr)
        .select(nme.apply)
        .appliedTo(quotes.nn)

    /** Helper methods to construct trees calling methods in `Quotes.reflect` based on the current `quotes` tree */
    private object reflect extends ReifiedReflect {
      def quotesTree = quotes.nn
    }

  end SpliceTransformer

end Splicing
