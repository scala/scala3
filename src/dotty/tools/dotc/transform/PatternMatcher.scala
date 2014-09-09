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

    def newSynthCaseLabel(name: String, tpe:Type) = ctx.newSymbol(ctx.owner, ctx.freshName(name).toTermName, Flags.Label, tpe)
      //NoSymbol.newLabel(freshName(name), NoPosition) setFlag treeInfo.SYNTH_CASE_FLAGS

    // codegen relevant to the structure of the translation (how extractors are combined)
    trait AbsCodegen {
      def matcher(scrut: Tree, scrutSym: Symbol, restpe: Type)(cases: List[Casegen => Tree], matchFailGen: Option[Symbol => Tree]): Tree

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

  trait OptimizedCodegen extends CodegenCore /*with TypedSubstitution*/ with MatchMonadInterface {
    override def codegen: AbsCodegen = optimizedCodegen

    // when we know we're targetting Option, do some inlining the optimizer won't do
    // for example, `o.flatMap(f)` becomes `if(o == None) None else f(o.get)`, similarly for orElse and guard
    //   this is a special instance of the advanced inlining optimization that takes a method call on
    //   an object of a type that only has two concrete subclasses, and inlines both bodies, guarded by an if to distinguish the two cases
    object optimizedCodegen extends CommonCodegen { //import CODE._

      /** Inline runOrElse and get rid of Option allocations
        *
        * runOrElse(scrut: scrutTp)(matcher): resTp = matcher(scrut) getOrElse ${catchAll(`scrut`)}
        * the matcher's optional result is encoded as a flag, keepGoing, where keepGoing == true encodes result.isEmpty,
        * if keepGoing is false, the result Some(x) of the naive translation is encoded as matchRes == x
        */
      def matcher(scrut: Tree, scrutSym: Symbol, restpe: Type)(cases: List[Casegen => Tree], matchFailGen: Option[Symbol => Tree]): Tree = {
        val matchRes = ctx.newSymbol(NoSymbol, ctx.freshName("x").toTermName, Flags.Synthetic | Flags.Param, restpe /*withoutAnnotations*/)
          //NoSymbol.newValueParameter(newTermName("x"), NoPosition, newFlags = SYNTHETIC) setInfo restpe.withoutAnnotations
        val mtype = MethodType(List("x".toTermName), List(restpe))(_=>restpe)
        val matchEnd = newSynthCaseLabel("matchEnd", mtype)
        val matchEndDef = DefDef(matchEnd, args => args.head.head)
        var lastSymbol: TermSymbol = null
        def newCaseSym = {
          lastSymbol = newSynthCaseLabel(ctx.freshName("case"), MethodType(Nil, restpe))
          lastSymbol
        }

        // must compute catchAll after caseLabels (side-effects nextCase)
        // catchAll.isEmpty iff no synthetic default case needed (the (last) user-defined case is a default)
        // if the last user-defined case is a default, it will never jump to the next case; it will go immediately to matchEnd
        val catchAllDef = matchFailGen.map({ matchFailGen =>
          DefDef(newCaseSym, _ => Block(List(matchEndDef), ref(matchEnd).appliedTo(matchFailGen(scrutSym))))
        }) // at most 1 element



        val caseDefs = cases.foldLeft[Tree](catchAllDef.getOrElse(matchEndDef)){ (acc: Tree, mkCase: Casegen => Tree) =>
          val nextCase = lastSymbol.orElse(matchEnd)


          DefDef(newCaseSym, _ => Block(List(acc), mkCase(new OptimizedCasegen(matchEnd, nextCase))))
        }


        // scrutSym == NoSymbol when generating an alternatives matcher
        // val scrutDef = scrutSym.fold(List[Tree]())(ValDef(_, scrut) :: Nil) // for alternatives

        // the generated block is taken apart in TailCalls under the following assumptions
        // the assumption is once we encounter a case, the remainder of the block will consist of cases
        // the prologue may be empty, usually it is the valdef that stores the scrut
        // val (prologue, cases) = stats span (s => !s.isInstanceOf[LabelDef])
        Block(
          List(caseDefs),
          ref(lastSymbol).appliedToNone
        )
      }

      class OptimizedCasegen(matchEnd: Symbol, nextCase: Symbol) extends CommonCodegen with Casegen {
        def matcher(scrut: Tree, scrutSym: Symbol, restpe: Type)(cases: List[Casegen => Tree], matchFailGen: Option[Symbol => Tree]): Tree =
          optimizedCodegen.matcher(scrut, scrutSym, restpe)(cases, matchFailGen)

        // only used to wrap the RHS of a body
        // res: T
        // returns MatchMonad[T]
        def one(res: Tree): Tree = ref(matchEnd) appliedTo res // a jump to a case label is special-cased in typedApply
        protected def zero: Tree = ref(nextCase) appliedToNone

        // prev: MatchMonad[T]
        // b: T
        // next: MatchMonad[U]
        // returns MatchMonad[U]
        def flatMap(prev: Tree, b: Symbol, next: Tree): Tree = {
          val prevSym = freshSym(prev.pos, prev.tpe, "o")
          Block(
            List(ValDef(prevSym, prev)),
            // must be isEmpty and get as we don't control the target of the call (prev is an extractor call)
            ifThenElseZero(
              ref(prevSym).select("isEmpty".toTermName).select(ctx.definitions.Boolean_!),
              /*Substitution(b, prevSym DOT vpmName.get)*/(next) // todo?
            )
          )
        }

        // cond: Boolean
        // res: T
        // nextBinder: T
        // next == MatchMonad[U]
        // returns MatchMonad[U]
        def flatMapCond(cond: Tree, res: Tree, nextBinder: Symbol, next: Tree): Tree = {
          val rest = Block(List(ValDef(nextBinder.asTerm, res)), next)
          ifThenElseZero(cond, rest)
        }

        // guardTree: Boolean
        // next: MatchMonad[T]
        // returns MatchMonad[T]
        def flatMapGuard(guardTree: Tree, next: Tree): Tree =
          ifThenElseZero(guardTree, next)

        def flatMapCondStored(cond: Tree, condSym: Symbol, res: Tree, nextBinder: Symbol, next: Tree): Tree =
          ifThenElseZero(cond, Block(
            List(Assign(ref(condSym), Literal(Constant(true))),
              Assign(ref(nextBinder), res)),
            next
          ))
      }

    }
  }
}