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
import dotty.tools.dotc.config.ScalaRelease.*

import scala.collection.mutable
import dotty.tools.dotc.core.Annotations._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.quoted._
import dotty.tools.dotc.transform.TreeMapWithStages._
import dotty.tools.dotc.typer.Inliner

import scala.annotation.constructorOnly

/** Translates quoted terms and types to `unpickleExprV2` or `unpickleType` method calls.
 *
 *  Transforms top level quote
 *   ```
 *   '{ ...
 *      @TypeSplice type X0 = {{ 0 | .. | contentsTpe0 | .. }}
 *      @TypeSplice type X2 = {{ 1 | .. | contentsTpe1 | .. }}
 *      val x1: U1 = ???
 *      val x2: U2 = ???
 *      ...
 *      {{{ 3 | x1 | contents0 | T0 }}} // hole
 *      ...
 *      {{{ 4 | x2 | contents1 | T1 }}} // hole
 *      ...
 *      {{{ 5 | x1, x2 | contents2 | T2 }}} // hole
 *      ...
 *    }
 *    ```
 *  to
 *    ```
 *     unpickleExprV2(
 *       pickled = [[ // PICKLED TASTY
 *       @TypeSplice type X0 // with bounds that do not contain captured types
 *       @TypeSplice type X1 // with bounds that do not contain captured types
 *         val x1 = ???
 *         val x2 = ???
 *         ...
 *      {{{ 0 | x1 | | T0 }}} // hole
 *      ...
 *      {{{ 1 | x2 | | T1 }}} // hole
 *      ...
 *      {{{ 2 | x1, x2 | | T2 }}} // hole
 *         ...
 *       ]],
 *       typeHole = (idx: Int, args: List[Any]) => idx match {
 *         case 0 => contentsTpe0.apply(args(0).asInstanceOf[Type[?]]) // beta reduced
 *         case 1 => contentsTpe1.apply(args(0).asInstanceOf[Type[?]]) // beta reduced
 *       },
 *       termHole = (idx: Int, args: List[Any], quotes: Quotes) => idx match {
 *         case 3 => content0.apply(args(0).asInstanceOf[Expr[U1]]).apply(quotes) // beta reduced
 *         case 4 => content1.apply(args(0).asInstanceOf[Expr[U2]]).apply(quotes) // beta reduced
 *         case 5 => content2.apply(args(0).asInstanceOf[Expr[U1]], args(1).asInstanceOf[Expr[U2]]).apply(quotes) // beta reduced
 *       },
 *     )
 *    ```
 *  and then performs the same transformation on any quote contained in the `content`s.
 *
 */
class PickleQuotes extends MacroTransform {
  import PickleQuotes._
  import tpd._

  override def phaseName: String = PickleQuotes.name

  override def description: String = PickleQuotes.description

  override def allowsImplicitSearch: Boolean = true

  override def checkPostCondition(tree: Tree)(using Context): Unit =
    tree match
      case tree: RefTree if !Inliner.inInlineMethod =>
        assert(!tree.symbol.isQuote)
        assert(!tree.symbol.isExprSplice)
      case _ : TypeDef if !Inliner.inInlineMethod =>
        assert(!tree.symbol.hasAnnotation(defn.QuotedRuntime_SplicedTypeAnnot),
          s"${tree.symbol} should have been removed by PickledQuotes because it has a @quoteTypeTag")
      case _ =>

  override def run(using Context): Unit =
    if (ctx.compilationUnit.needsStaging) super.run(using freshStagingContext)

  protected def newTransformer(using Context): Transformer = new Transformer {
    override def transform(tree: tpd.Tree)(using Context): tpd.Tree =
      tree match
        case Apply(Select(Apply(TypeApply(fn, List(tpt)), List(code)),nme.apply), List(quotes))
        if fn.symbol == defn.QuotedRuntime_exprQuote =>
          val (contents, codeWithHoles) = makeHoles(code)
          val sourceRef = Inliner.inlineCallTrace(ctx.owner, tree.sourcePos)
          val codeWithHoles2 = Inlined(sourceRef, Nil, codeWithHoles)
          val pickled = PickleQuotes(quotes, codeWithHoles2, contents, tpt.tpe, false)
          transform(pickled) // pickle quotes that are in the contents
        case Apply(TypeApply(_, List(tpt)), List(quotes)) if tree.symbol == defn.QuotedTypeModule_of =>
          tpt match
            case Select(t, _) if tpt.symbol == defn.QuotedType_splice =>
              // `Type.of[t.Underlying](quotes)`  --> `t`
              ref(t.symbol)(using ctx.withSource(tpt.source)).withSpan(tpt.span)
            case _ =>
              val (contents, tptWithHoles) = makeHoles(tpt)
              PickleQuotes(quotes, tptWithHoles, contents, tpt.tpe, true)
        case tree: DefDef if tree.symbol.is(Macro) =>
          // Shrink size of the tree. The methods have already been inlined.
          // TODO move to FirstTransform to trigger even without quotes
          cpy.DefDef(tree)(rhs = defaultValue(tree.rhs.tpe))
        case _: DefDef if tree.symbol.isInlineMethod =>
          tree
        case _ =>
          super.transform(tree)
  }

  private def makeHoles(tree: tpd.Tree)(using Context): (List[Tree], tpd.Tree) =

    class HoleContentExtractor extends Transformer:
      private val contents = List.newBuilder[Tree]
      override def transform(tree: tpd.Tree)(using Context): tpd.Tree =
        tree match
          case tree @ Hole(isTerm, _, _, content, _) =>
            if !content.isEmpty then
              contents += content
            val holeType =
              if isTerm then getTermHoleType(tree.tpe) else getTypeHoleType(tree.tpe)
            val hole = cpy.Hole(tree)(content = EmptyTree, TypeTree(holeType))
            if isTerm then Inlined(EmptyTree, Nil, hole).withSpan(tree.span) else hole
          case tree: DefTree =>
            val newAnnotations = tree.symbol.annotations.mapconserve { annot =>
              annot.derivedAnnotation(transform(annot.tree)(using ctx.withOwner(tree.symbol)))
            }
            tree.symbol.annotations = newAnnotations
            super.transform(tree)
          case _ =>
            super.transform(tree).withType(mapAnnots(tree.tpe))

      private def mapAnnots = new TypeMap { // TODO factor out duplicated logic in Splicing
        override def apply(tp: Type): Type = {
            tp match
              case tp @ AnnotatedType(underlying, annot) =>
                val underlying1 = this(underlying)
                derivedAnnotatedType(tp, underlying1, annot.derivedAnnotation(transform(annot.tree)))
              case _ => mapOver(tp)
        }
      }

      /** Remove references to local types that will not be defined in this quote */
      private def getTypeHoleType(using Context) = new TypeMap() {
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
      private def getTermHoleType(using Context) = new TypeMap() {
        override def apply(tp: Type): Type = tp match
          case tp @ TypeRef(NoPrefix, _) =>
            // reference to term with a type defined in outer quote
            getTypeHoleType(tp)
          case tp @ TermRef(NoPrefix, _) =>
            // widen term refs to terms defined in outer quote
            apply(tp.widenTermRefExpr)
          case tp =>
            mapOver(tp)
      }

      /** Get the contents of the transformed tree */
      def getContents() =
        val res = contents.result
        contents.clear()
        res
    end HoleContentExtractor

    val holeMaker = new HoleContentExtractor
    val newTree = holeMaker.transform(tree)
    (holeMaker.getContents(), newTree)


  end makeHoles

}

object PickleQuotes {
  import tpd._

  val name: String = "pickleQuotes"
  val description: String = "turn quoted trees into explicit run-time data structures"

  def apply(quotes: Tree, body: Tree, contents: List[Tree], originalTp: Type, isType: Boolean)(using Context) = {
    /** Helper methods to construct trees calling methods in `Quotes.reflect` based on the current `quotes` tree */
    object reflect extends ReifiedReflect {
      val quotesTree = quotes
    }

    /** Encode quote using Reflection.Literal
      *
      *  Generate the code
      *  ```scala
      *    quotes => quotes.reflect.TreeMethods.asExpr(
      *      quotes.reflect.Literal.apply(x$1.reflect.Constant.<typeName>.apply(<literalValue>))
      *    ).asInstanceOf[scala.quoted.Expr[<body.type>]]
      *  ```
      *  this closure is always applied directly to the actual context and the BetaReduce phase removes it.
      */
    def pickleAsLiteral(lit: Literal) = {
      val typeName = body.tpe.typeSymbol.name
      val literalValue =
        if lit.const.tag == Constants.NullTag || lit.const.tag == Constants.UnitTag then Nil
        else List(body)
      val constModule = lit.const.tag match
        case Constants.BooleanTag => defn. Quotes_reflect_BooleanConstant
        case Constants.ByteTag => defn. Quotes_reflect_ByteConstant
        case Constants.ShortTag => defn. Quotes_reflect_ShortConstant
        case Constants.IntTag => defn. Quotes_reflect_IntConstant
        case Constants.LongTag => defn. Quotes_reflect_LongConstant
        case Constants.FloatTag => defn. Quotes_reflect_FloatConstant
        case Constants.DoubleTag => defn. Quotes_reflect_DoubleConstant
        case Constants.CharTag => defn. Quotes_reflect_CharConstant
        case Constants.StringTag => defn. Quotes_reflect_StringConstant
        case Constants.UnitTag => defn. Quotes_reflect_UnitConstant
        case Constants.NullTag => defn. Quotes_reflect_NullConstant
        case Constants.ClazzTag => defn. Quotes_reflect_ClassOfConstant
      reflect.asExpr(body.tpe) {
        reflect.Literal {
          reflect.self
            .select(constModule)
            .select(nme.apply)
            .appliedToTermArgs(literalValue)
        }
      }
    }

    /** Encode quote using Reflection.Literal
      *
      *  Generate the code
      *  ```scala
      *    quotes => scala.quoted.ToExpr.{BooleanToExpr,ShortToExpr, ...}.apply(<literalValue>)(quotes)
      *  ```
      *  this closure is always applied directly to the actual context and the BetaReduce phase removes it.
      */
    def liftedValue(lit: Literal, lifter: Symbol) =
      val exprType = defn.QuotedExprClass.typeRef.appliedTo(body.tpe)
      ref(lifter).appliedToType(originalTp).select(nme.apply).appliedTo(lit).appliedTo(quotes)

    def pickleAsValue(lit: Literal) = {
      // TODO should all constants be pickled as Literals?
      // Should examine the generated bytecode size to decide and performance
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
      *    quotes => quotes.asInstanceOf[QuoteUnpickler].<unpickleExpr|unpickleType>[<type>](
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
        case xs => tpd.mkList(xs.map(x => Literal(Constant(x))), TypeTree(defn.StringType))

      // TODO split holes earlier into types and terms. This all holes in each category can have consecutive indices
      val (typeSplices, termSplices) = contents.zipWithIndex.partition {
        _._1.tpe.derivesFrom(defn.QuotedTypeClass)
      }

      // This and all closures in typeSplices are removed by the BetaReduce phase
      val types =
        if typeSplices.isEmpty then Literal(Constant(null)) // keep pickled quote without contents as small as possible
        else SeqLiteral(typeSplices.map(_._1), TypeTree(defn.QuotedTypeClass.typeRef.appliedTo(WildcardType)))

      // This and all closures in termSplices are removed by the BetaReduce phase
      val termHoles =
        if termSplices.isEmpty then Literal(Constant(null)) // keep pickled quote without contents as small as possible
        else
          Lambda(
            MethodType(
              List(nme.idx, nme.contents, nme.quotes).map(name => UniqueName.fresh(name).toTermName),
              List(defn.IntType, defn.SeqType.appliedTo(defn.AnyType), defn.QuotesClass.typeRef),
              defn.QuotedExprClass.typeRef.appliedTo(defn.AnyType)),
            args =>
              val cases = termSplices.map { case (splice, idx) =>
                val defn.FunctionOf(argTypes, defn.FunctionOf(quotesType :: _, _, _, _), _, _) = splice.tpe
                val rhs = {
                  val spliceArgs = argTypes.zipWithIndex.map { (argType, i) =>
                    args(1).select(nme.apply).appliedTo(Literal(Constant(i))).asInstance(argType)
                  }
                  val Block(List(ddef: DefDef), _) = splice
                  // TODO: beta reduce inner closure? Or wait until BetaReduce phase?
                  BetaReduce(ddef, spliceArgs).select(nme.apply).appliedTo(args(2).asInstance(quotesType))
                }
                CaseDef(Literal(Constant(idx)), EmptyTree, rhs)
              }
              cases match
                case CaseDef(_, _, rhs) :: Nil => rhs
                case _ => Match(args(0).annotated(New(ref(defn.UncheckedAnnot.typeRef))), cases)
          )

      val quoteClass = if isType then defn.QuotedTypeClass else defn.QuotedExprClass
      val quotedType = quoteClass.typeRef.appliedTo(originalTp)
      val lambdaTpe = MethodType(defn.QuotesClass.typeRef :: Nil, quotedType)
      val unpickleMeth =
        if isType then defn.QuoteUnpickler_unpickleTypeV2
        else defn.QuoteUnpickler_unpickleExprV2
      val unpickleArgs =
        if isType then List(pickledQuoteStrings, types)
        else List(pickledQuoteStrings, types, termHoles)
      quotes
        .asInstance(defn.QuoteUnpicklerClass.typeRef)
        .select(unpickleMeth).appliedToType(originalTp)
        .appliedToArgs(unpickleArgs).withSpan(body.span)
    }

    /** Encode quote using Reflection.TypeRepr.typeConstructorOf
      *
      *  Generate the code
      *  ```scala
      *    quotes.reflect.TypeReprMethods.asType(
      *      quotes.reflect.TypeRepr.typeConstructorOf(classOf[<type>]])
      *    ).asInstanceOf[scala.quoted.Type[<type>]]
      *  ```
      *  this closure is always applied directly to the actual context and the BetaReduce phase removes it.
      */
    def taggedType() =
      reflect.asType(body.tpe) {
        reflect.TypeRepr_typeConstructorOf(
          TypeApply(ref(defn.Predef_classOf.termRef), body :: Nil)
        )
      }

    def getLiteral(tree: tpd.Tree): Option[Literal] = tree match
      case tree: Literal => Some(tree)
      case Block(Nil, e) => getLiteral(e)
      case Inlined(_, Nil, e) => getLiteral(e)
      case _ => None

    if (isType) then
      if contents.isEmpty && body.symbol.isPrimitiveValueClass then taggedType()
      else pickleAsTasty()
    else
      getLiteral(body) match
        case Some(lit) => pickleAsValue(lit)
        case _ => pickleAsTasty()
  }

}
