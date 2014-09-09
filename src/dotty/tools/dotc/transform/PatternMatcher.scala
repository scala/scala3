package dotty.tools.dotc
package transform

import TreeTransforms._
import core.Denotations._
import core.SymDenotations._
import core.Contexts._
import core.Symbols._
import core.Types._
import core.Constants._
import core.StdNames._
import core.transform.Erasure.isUnboundedGeneric
import typer.ErrorReporting._
import ast.Trees._

import dotty.tools.dotc.util.Positions.Position
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags

/** This transform eliminates patterns. Right now it's a dummy.
 *  Awaiting the real pattern matcher.
 */
class PatternMatcher extends MiniPhaseTransform {
  import dotty.tools.dotc.ast.tpd._

  implicit val ctx: Context = ???

  def name: String = "patternMatcher"

  /*override def transformCaseDef(tree: CaseDef)(implicit ctx: Context, info: TransformerInfo): Tree =
    cpy.CaseDef(tree, Literal(Constant("<eliminated pattern>")), tree.guard, tree.body)*/


  /*case Try(block, catches, finalizer) =>
  treeCopy.Try(tree, transform(block), translator.translateTry(transformTrees(catches).asInstanceOf[List[CaseDef]], tree.tpe, tree.pos), transform(finalizer))
  case _ => super.transform(tree)*/
  /*override def transformMatch(tree: tpd.Match)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    case Match(sel, cases) =>
      val origTp = tree.tpe
      // setType origTp intended for CPS -- TODO: is it necessary?
      val translated = translator.translateMatch(treeCopy.Match(tree, transform(sel), transformTrees(cases).asInstanceOf[List[CaseDef]]))
      try {
        localTyper.typed(translated) setType origTp
      } catch {
        case x: (Types#TypeError) =>
          // TODO: this should never happen; error should've been reported during type checking
          unit.error(tree.pos, "error during expansion of this match (this is a scalac bug).\nThe underlying error was: "+ x.msg)
          translated
      }
  }*/


  def translator = {
    new OptimizingMatchTranslator/*(localTyper)*/
  }

  class OptimizingMatchTranslator/*(val typer: analyzer.Typer)*/ /*extends MatchTranslator
  with MatchOptimizer*/

  trait Debugging {

    // TODO: the inliner fails to inline the closures to debug.patmat unless the method is nested in an object
    object debug {
      //val printPatmat = global.settings.Ypatmatdebug.value
      final def patmat(s: => String) = /*if (printPatmat) Console.err.*/
        println(s)
      final def patmatResult[T](s: => String)(result: T): T = {
        /*if (printPatmat) Console.err.*/
        println(s + ": " + result)
        result
      }
    }
  }

  trait MatchMonadInterface {
    // val typer: Typer
    def matchOwner(implicit ctx: Context) = ctx.owner
    def pureType(tp: Type): Type = tp

    def reportUnreachable(pos: Position) = {
        ctx.warning("unreachable code", pos)
    }
    def reportMissingCases(pos: Position, counterExamples: List[String]) = {
      val ceString =
        if (counterExamples.tail.isEmpty) "input: " + counterExamples.head
        else "inputs: " + counterExamples.mkString(", ")

      ctx.warning("match may not be exhaustive.\nIt would fail on the following "+ ceString, pos)
    }
  }

  trait CodegenCore extends MatchMonadInterface {
    private var ctr = 0
    def freshName(prefix: String) = ctx.freshName(prefix).toTermName

    // assert(owner ne null); assert(owner ne NoSymbol)
    def freshSym(pos: Position, tp: Type = NoType, prefix: String = "x") =
      ctx.newSymbol(ctx.owner, freshName(prefix) ,Flags.Synthetic, tp, coord = pos)

    def newSynthCaseLabel(name: String) = ???
      //NoSymbol.newLabel(freshName(name), NoPosition) setFlag treeInfo.SYNTH_CASE_FLAGS

    // codegen relevant to the structure of the translation (how extractors are combined)
    trait AbsCodegen {
      def matcher(scrut: Tree, scrutSym: Symbol, restpe: Type)(cases: List[Casegen => Tree], matchFailGen: Option[Tree => Tree]): Tree

      // local / context-free
      def _asInstanceOf(b: Symbol, tp: Type): Tree
      def _equals(checker: Tree, binder: Symbol): Tree
      def _isInstanceOf(b: Symbol, tp: Type): Tree
      def drop(tgt: Tree)(n: Int): Tree
      def index(tgt: Tree)(i: Int): Tree
      def mkZero(tp: Type): Tree
      def tupleSel(binder: Symbol)(i: Int): Tree
    }

    // structure
    trait Casegen extends AbsCodegen {
      def one(res: Tree): Tree

      def flatMap(prev: Tree, b: Symbol, next: Tree): Tree
      def flatMapCond(cond: Tree, res: Tree, nextBinder: Symbol, next: Tree): Tree
      def flatMapGuard(cond: Tree, next: Tree): Tree
      def ifThenElseZero(c: Tree, thenp: Tree): Tree =
        If(c, thenp, zero)
      protected def zero: Tree
    }

    def codegen: AbsCodegen

    abstract class CommonCodegen extends AbsCodegen {
      def fun(arg: TermSymbol, body: Tree): Tree     =
        DefDef(arg, body)

      def tupleSel(binder: Symbol)(i: Int): Tree = ref(binder).select(nme.productAccessorName(i)) // make tree that accesses the i'th component of the tuple referenced by binder
      def index(tgt: Tree)(i: Int): Tree         = tgt.appliedTo(Literal(Constant(i)))

      // Right now this blindly calls drop on the result of the unapplySeq
      // unless it verifiably has no drop method (this is the case in particular
      // with Array.) You should not actually have to write a method called drop
      // for name-based matching, but this was an expedient route for the basics.
      def drop(tgt: Tree)(n: Int): Tree = {
        def callDirect   = tgt.select(nme.drop).appliedTo(Literal(Constant(n)))
        def callRuntime  = ref(ctx.definitions.traversableDropMethod).appliedTo(tgt, Literal(Constant(n)))

        def needsRuntime = (tgt.tpe ne null) && tgt.tpe.baseTypeRef(ctx.definitions.SeqType.classSymbol).member(nme.drop).exists /*typeOfMemberNamedDrop(tgt.tpe) == NoType*/

        if (needsRuntime) callRuntime else callDirect
      }

      // NOTE: checker must be the target of the ==, that's the patmat semantics for ya
      def _equals(checker: Tree, binder: Symbol): Tree = checker.select(defn.Any_equals).appliedTo(ref(binder))

      // the force is needed mainly to deal with the GADT typing hack (we can't detect it otherwise as tp nor pt need contain an abstract type, we're just casting wildly)
      def _asInstanceOf(b: Symbol, tp: Type): Tree = if (b.info <:< tp) ref(b) else ref(b).select(defn.Any_asInstanceOf).appliedToType(tp)
      def _isInstanceOf(b: Symbol, tp: Type): Tree = ref(b).select(defn.Any_isInstanceOf).appliedToType(tp)

      def mkZero(tp: Type): Tree = initValue(tp)
    }
  }
}