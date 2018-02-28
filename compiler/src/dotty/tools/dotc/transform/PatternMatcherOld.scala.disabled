package dotty.tools.dotc
package transform

import scala.language.postfixOps

import MegaPhase._
import core.Denotations._
import core.SymDenotations._
import core.Contexts._
import core.Symbols._
import core.Types._
import core.Constants._
import core.StdNames._
import core.NameKinds._
import dotty.tools.dotc.ast.{untpd, TreeTypeMap, tpd}
import dotty.tools.dotc.core
import dotty.tools.dotc.core.DenotTransformers.DenotTransformer
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.{TypeApplications, Flags}
import dotty.tools.dotc.typer.Applications
import dotty.tools.dotc.util.Positions
import typer.ErrorReporting._
import ast.Trees._
import Applications._
import TypeApplications._
import SymUtils._, core.NameOps._
import core.Mode
import patmat._

import dotty.tools.dotc.util.Positions.Position
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags

/** This phase rewrites pattern matches.
 *  FIXME: A more detailed explanation would be good.
 */
class PatternMatcherOld extends MiniPhase with DenotTransformer {
  import dotty.tools.dotc.ast.tpd._

  override def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation = ref

  override def runsAfter = Set(classOf[ElimRepeated])

  override def runsAfterGroupsOf = Set(classOf[TailRec]) // tailrec is not capable of reversing the patmat tranformation made for tree

  override def phaseName = "patternMatcher"

  private[this] var _id = 0 // left for debuging

  override def transformMatch(tree: Match)(implicit ctx: Context): Tree = {
    val translated = new Translator()(ctx).translator.translateMatch(tree)

    // check exhaustivity and unreachability
    val engine = new SpaceEngine
    if (engine.checkable(tree)) {
      engine.checkExhaustivity(tree)
      engine.checkRedundancy(tree)
    }

    translated.ensureConforms(tree.tpe)
  }

  class Translator(implicit ctx: Context) {

  def translator = {
    new OptimizingMatchTranslator/*(localTyper)*/
  }

  class OptimizingMatchTranslator extends MatchOptimizer/*(val typer: analyzer.Typer)*/ with MatchTranslator

  trait CodegenCore {

    // assert(owner ne null); assert(owner ne NoSymbol)
    def freshSym(pos: Position, tp: Type = NoType, unique: UniqueNameKind = PatMatStdBinderName, owner: Symbol = ctx.owner) = {
      ctx.newSymbol(owner, unique.fresh(), Flags.Synthetic | Flags.Case, tp, coord = pos)
    }

    def newSynthCaseLabel(unique: UniqueNameKind, tpe: Type, owner: Symbol = ctx.owner) =
      ctx.newSymbol(owner, unique.fresh(), Flags.Label | Flags.Synthetic | Flags.Method, tpe).asTerm
      //NoSymbol.newLabel(freshName(name), NoPosition) setFlag treeInfo.SYNTH_CASE_FLAGS

    // codegen relevant to the structure of the translation (how extractors are combined)
    trait AbsCodegen {
      def matcher(scrut: Tree, scrutSym: Symbol, restpe: Type)(cases: List[Casegen => Tree], matchFailGen: Option[Symbol => Tree]): Tree

      // local / context-free

      /* cast b to tp */
      def _asInstanceOf(b: Symbol, tp: Type): Tree
      /* a check `checker` == binder */
      def _equals(checker: Tree, binder: Symbol): Tree
      /* b.isIsInstanceOf[tp] */
      def _isInstanceOf(b: Symbol, tp: Type): Tree
      /* tgt is expected to be a Seq, call tgt.drop(n) */
      def drop(tgt: Tree)(n: Int): Tree
      /* tgt is expected to have method apply(int), call tgt.apply(i) */
      def index(tgt: Tree)(i: Int): Tree
      /* make tree that accesses the i'th component of the tuple referenced by binder */
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
      def tupleSel(binder: Symbol)(i: Int): Tree = ref(binder).select(nme.productAccessorName(i))
      def index(tgt: Tree)(i: Int): Tree         = {
        if (i > 0) tgt.select(defn.Seq_apply).appliedTo(Literal(Constant(i)))
        else tgt.select(defn.Seq_head).ensureApplied
      }

      // Right now this blindly calls drop on the result of the unapplySeq
      // unless it verifiably has no drop method (this is the case in particular
      // with Array.) You should not actually have to write a method called drop
      // for name-based matching, but this was an expedient route for the basics.
      def drop(tgt: Tree)(n: Int): Tree = {
        def callDirect   = tgt.select(nme.drop).appliedTo(Literal(Constant(n)))
        def callRuntime  = ref(defn.ScalaRuntime_drop).appliedTo(tgt, Literal(Constant(n)))

        def needsRuntime = !(tgt.tpe derivesFrom defn.SeqClass) /*typeOfMemberNamedDrop(tgt.tpe) == NoType*/

        if (needsRuntime) callRuntime else callDirect
      }

      // NOTE: checker must be the target of the ==, that's the patmat semantics for ya
      def _equals(checker: Tree, binder: Symbol): Tree =
        tpd.applyOverloaded(checker, nme.EQ, List(ref(binder)), List.empty, defn.BooleanType)

      // the force is needed mainly to deal with the GADT typing hack (we can't detect it otherwise as tp nor pt need contain an abstract type, we're just casting wildly)
      def _asInstanceOf(b: Symbol, tp: Type): Tree = ref(b).ensureConforms(tp) // andType here breaks t1048
      def _isInstanceOf(b: Symbol, tp: Type): Tree = ref(b).select(defn.Any_isInstanceOf).appliedToType(tp)
    }
  }

  object Rebindings {
    def apply(from: Symbol, to: Symbol) = new Rebindings(List(from), List(ref(to)))
    // requires sameLength(from, to)
    def apply(from: List[Symbol], to: List[Tree]) =
      if (from nonEmpty) new Rebindings(from, to) else NoRebindings
  }

  class Rebindings(val lhs: List[Symbol], val rhs: List[Tree]) {
    def >>(other: Rebindings) = {
      if (other eq NoRebindings) this
      else if (this eq NoRebindings) other
      else {
        assert((lhs.toSet ++ other.lhs.toSet).size == lhs.length + other.lhs.length, "no double assignments")
        new Rebindings(this.lhs ++ other.lhs, this.rhs ++ other.rhs)
      }
    }

    def emitValDefs: List[ValDef] = {
      (lhs, rhs).zipped.map((symbol, tree) => ValDef(symbol.asTerm, tree.ensureConforms(symbol.info)))
    }
  }
  object NoRebindings extends Rebindings(Nil, Nil)

  trait OptimizedCodegen extends CodegenCore {
    override def codegen: AbsCodegen = optimizedCodegen

    // when we know we're targetting Option, do some inlining the optimizer won't do
    // for example, `o.flatMap(f)` becomes `if (o == None) None else f(o.get)`, similarly for orElse and guard
    //   this is a special instance of the advanced inlining optimization that takes a method call on
    //   an object of a type that only has two concrete subclasses, and inlines both bodies, guarded by an if to distinguish the two cases
    object optimizedCodegen extends CommonCodegen {

      /** Inline runOrElse and get rid of Option allocations
        *
        * runOrElse(scrut: scrutTp)(matcher): resTp = matcher(scrut) getOrElse ${catchAll(`scrut`)}
        * the matcher's optional result is encoded as a flag, keepGoing, where keepGoing == true encodes result.isEmpty,
        * if keepGoing is false, the result Some(x) of the naive translation is encoded as matchRes == x
        */
      def matcher(scrut: Tree, scrutSym: Symbol, restpe: Type)(cases: List[Casegen => Tree], matchFailGen: Option[Symbol => Tree]): Tree = {
        //val matchRes = ctx.newSymbol(NoSymbol, ctx.freshName("matchRes").toTermName, Flags.Synthetic | Flags.Param | Flags.Label | Flags.Method, restpe /*withoutAnnotations*/)
          //NoSymbol.newValueParameter(newTermName("x"), NoPosition, newFlags = SYNTHETIC) setInfo restpe.withoutAnnotations


        val caseSyms: List[TermSymbol] = cases.scanLeft(ctx.owner.asTerm)((curOwner, nextTree) =>
          newSynthCaseLabel(PatMatCaseName, MethodType(Nil, restpe), curOwner)).tail

        // must compute catchAll after caseLabels (side-effects nextCase)
        // catchAll.isEmpty iff no synthetic default case needed (the (last) user-defined case is a default)
        // if the last user-defined case is a default, it will never jump to the next case; it will go immediately to matchEnd
        val catchAllDef = matchFailGen.map { _(scrutSym) }
          .getOrElse(Throw(New(defn.MatchErrorType, List(ref(scrutSym)))))

        val matchFail = newSynthCaseLabel(PatMatMatchFailName, MethodType(Nil, restpe))
        val catchAllDefBody = DefDef(matchFail, catchAllDef)

        val nextCases = (caseSyms.tail ::: List(matchFail)).map(ref(_).ensureApplied)
        val caseDefs = (cases zip caseSyms zip nextCases).foldRight[Tree](catchAllDefBody) {
          // dotty deviation
          //case (((mkCase, sym), nextCase), acc) =>
          (x: (((Casegen => Tree), TermSymbol), Tree), acc: Tree) => x match {
            case ((mkCase, sym), nextCase) =>
              val body = mkCase(new OptimizedCasegen(nextCase)).ensureConforms(restpe)

              DefDef(sym, _ => Block(List(acc), body))
          }
        }

        // scrutSym == NoSymbol when generating an alternatives matcher
        // val scrutDef = scrutSym.fold(List[Tree]())(ValDef(_, scrut) :: Nil) // for alternatives

        Block(List(caseDefs), ref(caseSyms.head).ensureApplied)
      }

      class OptimizedCasegen(nextCase: Tree) extends CommonCodegen with Casegen {
        def matcher(scrut: Tree, scrutSym: Symbol, restpe: Type)(cases: List[Casegen => Tree], matchFailGen: Option[Symbol => Tree]): Tree =
          optimizedCodegen.matcher(scrut, scrutSym, restpe)(cases, matchFailGen)

        // only used to wrap the RHS of a body
        // res: T
        // returns MatchMonad[T]
        def one(res: Tree): Tree = /*ref(matchEnd) appliedTo*/ res // a jump to a case label is special-cased in typedApply
        protected def zero: Tree = nextCase

        // prev: MatchMonad[T]
        // b: T
        // next: MatchMonad[U]
        // returns MatchMonad[U]
        def flatMap(prev: Tree, b: Symbol, next: Tree): Tree = {
          val resultArity = productArity(b.info)
          if (isProductMatch(prev.tpe, resultArity)) {
            val nullCheck: Tree = prev.select(defn.Object_ne).appliedTo(Literal(Constant(null)))
            ifThenElseZero(
              nullCheck,
              Block(
                List(ValDef(b.asTerm, prev)),
                next //Substitution(b, ref(prevSym))(next)
              )
            )
          }
          else {
            val getDenot = extractorMember(prev.tpe, nme.get)
            val isEmptyDenot = extractorMember(prev.tpe, nme.isEmpty)
            assert(getDenot.exists && isEmptyDenot.exists, i"${prev.tpe}")

            val tmpSym = freshSym(prev.pos, prev.tpe, PatMatOName)
            val prevValue = ref(tmpSym).select(getDenot.symbol).ensureApplied

            Block(
              List(ValDef(tmpSym, prev)),
              // must be isEmpty and get as we don't control the target of the call (prev is an extractor call)
              ifThenElseZero(
                ref(tmpSym).select(isEmptyDenot.symbol).select(defn.Boolean_!),
                Block(List(ValDef(b.asTerm, prevValue)), next)
              )
            )
          }
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
  /*final*/ case class Suppression(exhaustive: Boolean, unreachable: Boolean)
  object Suppression {
    val NoSuppression = Suppression(false, false)
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // the making of the trees
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait TreeMakers extends CodegenCore {
    def optimizeCases(prevBinder: Symbol, cases: List[List[TreeMaker]], pt: Type): (List[List[TreeMaker]], List[Tree])
    def analyzeCases(prevBinder: Symbol, cases: List[List[TreeMaker]], pt: Type, suppression: Suppression): Unit = {}

    def emitSwitch(scrut: Tree, scrutSym: Symbol, cases: List[List[TreeMaker]], pt: Type, matchFailGenOverride: Option[Symbol => Tree], unchecked: Boolean): Option[Tree] = {
      // TODO Deal with guards?

      def isSwitchableType(tpe: Type): Boolean =
        (tpe isRef defn.IntClass) ||
        (tpe isRef defn.ByteClass) ||
        (tpe isRef defn.ShortClass) ||
        (tpe isRef defn.CharClass)

      object IntEqualityTestTreeMaker {
        def unapply(treeMaker: EqualityTestTreeMaker): Option[Int] = treeMaker match {
          case EqualityTestTreeMaker(`scrutSym`, _, Literal(const), _) =>
            if (const.isIntRange) Some(const.intValue)
            else None
          case _ =>
            None
        }
      }

      def isSwitchCase(treeMakers: List[TreeMaker]): Boolean = treeMakers match {
        // case 5 =>
        case List(IntEqualityTestTreeMaker(_), _: BodyTreeMaker) =>
          true

        // case 5 | 6 =>
        case List(AlternativesTreeMaker(`scrutSym`, alts, _), _: BodyTreeMaker) =>
          alts.forall {
            case List(IntEqualityTestTreeMaker(_)) => true
            case _ => false
          }

        // case _ =>
        case List(_: BodyTreeMaker) =>
          true

        /* case x @ pat =>
         * This includes:
         *   case x =>
         *   case x @ 5 =>
         *   case x @ (5 | 6) =>
         */
        case (_: SubstOnlyTreeMaker) :: rest =>
          isSwitchCase(rest)

        case _ =>
          false
      }

      /* (Nil, body) means that `body` is the default case
       * It's a bit hacky but it simplifies manipulations.
       */
      def extractSwitchCase(treeMakers: List[TreeMaker]): (List[Int], BodyTreeMaker) = (treeMakers: @unchecked) match {
        // case 5 =>
        case List(IntEqualityTestTreeMaker(intValue), body: BodyTreeMaker) =>
          (List(intValue), body)

        // case 5 | 6 =>
        case List(AlternativesTreeMaker(_, alts, _), body: BodyTreeMaker) =>
          val intValues = alts.map { alt =>
            (alt: @unchecked) match {
              case List(IntEqualityTestTreeMaker(intValue)) => intValue
            }
          }
          (intValues, body)

        // case _ =>
        case List(body: BodyTreeMaker) =>
          (Nil, body)

        // case x @ pat =>
        case (_: SubstOnlyTreeMaker) :: rest =>
          /* Rebindings have been propagated, so the eventual body in `rest`
           * contains all the necessary information. The substitution can be
           * dropped at this point.
           */
          extractSwitchCase(rest)
      }

      def doOverlap(a: List[Int], b: List[Int]): Boolean =
        a.exists(b.contains _)

      def makeSwitch(valuesToCases: List[(List[Int], BodyTreeMaker)]): Tree = {
        def genBody(body: BodyTreeMaker): Tree = {
          val valDefs = body.rebindings.emitValDefs
          if (valDefs.isEmpty) body.body
          else Block(valDefs, body.body)
        }

        val intScrut =
          if (pt isRef defn.IntClass) ref(scrutSym)
          else Select(ref(scrutSym), nme.toInt)

        val (normalCases, defaultCaseAndRest) = valuesToCases.span(_._1.nonEmpty)

        val newCases = for {
          (values, body) <- normalCases
        } yield {
          val literals = values.map(v => Literal(Constant(v)))
          val pat =
            if (literals.size == 1) literals.head
            else Alternative(literals)
          CaseDef(pat, EmptyTree, genBody(body))
        }

        val catchAllDef = {
          if (defaultCaseAndRest.isEmpty) {
            matchFailGenOverride.fold[Tree](
                Throw(New(defn.MatchErrorType, List(ref(scrutSym)))))(
                _(scrutSym))
          } else {
            /* After the default case, assuming the IR even allows anything,
             * things are unreachable anyway and can be removed.
             */
            genBody(defaultCaseAndRest.head._2)
          }
        }
        val defaultCase = CaseDef(Underscore(defn.IntType), EmptyTree, catchAllDef)

        Match(intScrut, newCases :+ defaultCase)
      }

      val dealiased = scrut.tpe.widenDealias
      if (isSwitchableType(dealiased) && cases.forall(isSwitchCase)) {
        val valuesToCases = cases.map(extractSwitchCase)
        val values = valuesToCases.map(_._1)
        if (values.tails.exists { tail => tail.nonEmpty && tail.tail.exists(doOverlap(_, tail.head)) }) {
          // TODO Deal with overlapping cases (mostly useless without guards)
          None
        } else {
          Some(makeSwitch(valuesToCases))
        }
      } else {
        if (dealiased hasAnnotation defn.SwitchAnnot)
          ctx.warning("failed to emit switch for `@switch` annotated match", scrut.pos)
        None
      }
    }

    // for catch (no need to customize match failure)
    def emitTypeSwitch(bindersAndCases: List[(Symbol, List[TreeMaker])], pt: Type): Option[List[CaseDef]] =
      None // todo

    abstract class TreeMaker {
      def pos: Position

      private[this] var currSub: Rebindings = null

      /** captures the scope and the value of the bindings in patterns
        * important *when* the substitution happens (can't accumulate and do at once after the full matcher has been constructed)
        */
      def rebindings: Rebindings =
        if (currSub eq null) introducedRebindings
        else currSub

      protected def introducedRebindings: Rebindings

      private[TreeMakers] def incorporateOuterRebinding(outerSubst: Rebindings): Unit = {
        if (currSub ne null) {
          ctx.debuglog("BUG: incorporateOuterRebinding called more than once for " + ((this, currSub, outerSubst)))
          if (ctx.debug) Thread.dumpStack()
        }
        else currSub = outerSubst >> rebindings
      }

      /** The substitution that specifies the trees that compute the values of the subpattern binders.
        *
        * Should not be used to perform actual substitution!
        * Only used to reason symbolically about the values the subpattern binders are bound to.
        * See TreeMakerToCond#updateSubstitution.
        *
        * Overridden in PreserveSubPatBinders to pretend it replaces the subpattern binders by subpattern refs
        * (Even though we don't do so anymore -- see SI-5158, SI-5739 and SI-6070.)
        *
        * TODO: clean this up, would be nicer to have some higher-level way to compute
        * the binders bound by this tree maker and the symbolic values that correspond to them
        */
      def subPatternsAsRebindings: Rebindings = rebindings

      // build Tree that chains `next` after the current extractor
      def chainBefore(next: Tree)(casegen: Casegen): Tree
    }

    sealed trait NoNewBinders extends TreeMaker {
      protected val introducedRebindings: Rebindings = NoRebindings
    }

    case class TrivialTreeMaker(tree: Tree) extends TreeMaker with NoNewBinders {
      def pos = tree.pos

      def chainBefore(next: Tree)(casegen: Casegen): Tree = tree
    }

    case class BodyTreeMaker(body: Tree, matchPt: Type) extends TreeMaker with NoNewBinders {
      def pos = body.pos

      def chainBefore(next: Tree)(casegen: Casegen): Tree = // assert(next eq EmptyTree)
        /*atPos(body.pos)*/(casegen.one(body)) // since SubstOnly treemakers are dropped, need to do it here
      override def toString = "B" + ((body, matchPt))
    }

    /**
     * In scalac for such block
     *   x match {
     *     case d => <body>
     *   }
     *
     * d inside <body> was to be substitued by x.
     *
     * In dotty, SubstOnlyTreeMakers instead generate normal ValDef,
     *   and does not create a new substitution.
     *
     * This was done for several reasons:
     *  1) it is a lot easyer to Y-check,
     *     as d type could be used in <body>.
     *  2) it would simplify debugging of the generated code as
     *     this works also for nested patterns, and previously they used unreadable names
     *  3) It showed better(~30%), performance,
     *     Rebuilding tree and propagating types was taking substantial time.
     */
    case class SubstOnlyTreeMaker(prevBinder: Symbol, nextBinder: Symbol) extends TreeMaker {
      val pos = Positions.NoPosition

      val introducedRebindings = Rebindings(prevBinder, nextBinder)
      def chainBefore(next: Tree)(casegen: Casegen): Tree = next
      //override def toString = "S" + localSubstitution
    }

    sealed abstract class FunTreeMaker extends TreeMaker {
      val nextBinder: Symbol
      def pos = nextBinder.pos
    }

    sealed abstract class CondTreeMaker extends FunTreeMaker {
      val prevBinder: Symbol
      val nextBinderTp: Type
      val cond: Tree
      val res: Tree

      val nextBinder: Symbol
      lazy val introducedRebindings = /*
        if (nextBinder ne prevBinder) Rebindings(prevBinder, nextBinder)
        else */ NoRebindings

      def chainBefore(next: Tree)(casegen: Casegen): Tree =
        if (prevBinder ne nextBinder) // happens when typeTest is known to succeed
          /*atPos(pos)(*/casegen.flatMapCond(cond, res, nextBinder, next)//)
        else casegen.flatMapGuard(cond, next)
    }

    // unless we're optimizing, emit local variable bindings for all subpatterns of extractor/case class patterns
    protected val debugInfoEmitVars = true //!settings.optimise.value

    /**
     * Tree maker that captures sub pattern values during pattern match.
     */
    sealed trait PreserveSubPatBinders extends TreeMaker {
      val subPatBinders: List[Symbol] // captured values
      val subPatRefs: List[Tree] // trees that will replace references to subPatBinders
      val ignoredSubPatBinders: Set[Symbol] // ignored as they aren't used in body of pattern

      // unless `debugInfoEmitVars`, this set should contain the bare minimum for correctness
      // mutable case class fields need to be stored regardless (SI-5158, SI-6070) -- see override in ProductExtractorTreeMaker
      // sub patterns bound to wildcard (_) are never stored as they can't be referenced
      // dirty debuggers will have to get dirty to see the wildcards
      lazy val storedBinders: Set[Symbol] =
        (if (debugInfoEmitVars) subPatBinders.toSet else Set.empty) ++ extraStoredBinders -- ignoredSubPatBinders

      // e.g., mutable fields of a case class in ProductExtractorTreeMaker
      def extraStoredBinders: Set[Symbol]

      def emitVars = storedBinders.nonEmpty

      lazy val storedSubsted = (subPatBinders, subPatRefs).zipped.partition{ case (sym, _) => storedBinders(sym) }

      def stored = storedSubsted._1

      def substed = storedSubsted._2

      // dd: this didn't yet trigger error. But I believe it would. if this causes double denition of symbol error this can be replaced with NoRebindings
      protected lazy val introducedRebindings:  Rebindings = if (!emitVars) Rebindings(subPatBinders, subPatRefs)
      else {
        val (subPatBindersSubstituted, subPatRefsSubstituted) = substed.unzip
        Rebindings(subPatBindersSubstituted.toList, subPatRefsSubstituted.toList)
      }

      /** The substitution that specifies the trees that compute the values of the subpattern binders.
        *
        * We pretend to replace the subpattern binders by subpattern refs
        * (Even though we don't do so anymore -- see SI-5158, SI-5739 and SI-6070.)
        */
      override def subPatternsAsRebindings =
        Rebindings(subPatBinders, subPatRefs) >> super.subPatternsAsRebindings

      def bindSubPats(in: Tree): Tree =
        if (!emitVars) in
        else {
          // binders in `subPatBindersStored` that are referenced by tree `in`
          val usedBinders = new collection.mutable.HashSet[Symbol]()
          // all potentially stored subpat binders
          val potentiallyStoredBinders = stored.unzip._1.toSet
          // compute intersection of all symbols in the tree `in` and all potentially stored subpat binders
          new DeepFolder[Unit]((x: Unit, t: Tree) =>
            if (potentiallyStoredBinders(t.symbol)) usedBinders += t.symbol).apply((), in)

          if (usedBinders.isEmpty) in
          else {
            // only store binders actually used
            val (subPatBindersStored, subPatRefsStored) = stored.filter{case (b, _) => usedBinders(b)}.unzip

            Block((subPatBindersStored.toList, subPatRefsStored.toList).zipped.map((bind, ref) => {
              // required in case original pattern had a more precise type
              // eg case s@"foo" =>  would be otherwise translated to s with type String instead of String("foo")
              def refTpeWiden = ref.tpe.widen
              def bindInfoWiden = bind.info.widen
              def loc = bind.showFullName
              if (!(ref.tpe <:< bind.info.widen)) {
                ctx.debuglog(s"here ${bind.showFullName} expected: ${bindInfoWiden.show} got: ${refTpeWiden.show}")
              }
              val refCasted = ref.ensureConforms(bind.info)
              ValDef(bind.asTerm, refCasted)
            }), in)
          }
        }
    }

    /**
     * Make a TreeMaker that will result in an extractor call specified by `extractor`
     * the next TreeMaker (here, we don't know which it'll be) is chained after this one by flatMap'ing
     * a function with binder `nextBinder` over our extractor's result
     * the function's body is determined by the next TreeMaker
     * (furthermore, the interpretation of `flatMap` depends on the codegen instance we're using).
     *
     * The values for the subpatterns, as computed by the extractor call in `extractor`,
     * are stored in local variables that re-use the symbols in `subPatBinders`.
     * This makes extractor patterns more debuggable (SI-5739).
     */
    case class ExtractorTreeMaker(extractor: Tree, extraCond: Option[Tree], nextBinder: Symbol)(
      val subPatBinders: List[Symbol],
      val subPatRefs: List[Tree],
      extractorReturnsBoolean: Boolean,
      val checkedLength: Option[Int],
      val prevBinder: Symbol,
      val ignoredSubPatBinders: Set[Symbol]
    ) extends FunTreeMaker with PreserveSubPatBinders {

      def extraStoredBinders: Set[Symbol] = Set()

      ctx.debuglog(s"""
        |ExtractorTreeMaker($extractor, $extraCond, $nextBinder) {
        |  $subPatBinders
        |  $subPatRefs
        |  $extractorReturnsBoolean
        |  $checkedLength
        |  $prevBinder
        |  $ignoredSubPatBinders
        |}""".stripMargin)

      def chainBefore(next: Tree)(casegen: Casegen): Tree = {
        val condAndNext = extraCond match {
          case Some(cond: Tree) =>
            casegen.ifThenElseZero(cond, bindSubPats(next))
          case _ =>
            bindSubPats(next)
        }

        if (extractorReturnsBoolean) casegen.flatMapCond(extractor, unitLiteral, nextBinder, condAndNext)
        else casegen.flatMap(extractor, nextBinder, condAndNext) // getType?
      }

      override def toString = "X" + ((extractor, nextBinder.name))
    }

    object IrrefutableExtractorTreeMaker {
      // will an extractor with unapply method of methodtype `tp` always succeed?
      // note: this assumes the other side-conditions implied by the extractor are met
      // (argument of the right type, length check succeeds for unapplySeq,...)
      def irrefutableExtractorType(tp: Type): Boolean = tp.resultType.dealias match {
        // case TypeRef(_, SomeClass, _) => true todo
        // probably not useful since this type won't be inferred nor can it be written down (yet)
        // case ConstantTrue => true todo
        case _            => false
      }

      def unapply(xtm: ExtractorTreeMaker): Option[(Tree, Symbol)] = xtm match {
        case ExtractorTreeMaker(extractor, None, nextBinder) if irrefutableExtractorType(extractor.tpe) =>
          Some((extractor, nextBinder))
        case _ =>
          None
      }
    }

    object TypeTestTreeMaker {
      // factored out so that we can consistently generate other representations of the tree that implements the test
      // (e.g. propositions for exhaustivity and friends, boolean for isPureTypeTest)
      trait TypeTestCondStrategy {
        type Result

        def outerTest(testedBinder: Symbol, expectedTp: Type): Result
        // TODO: can probably always widen
        def typeTest(testedBinder: Symbol, expectedTp: Type): Result
        def nonNullTest(testedBinder: Symbol): Result
        def equalsTest(pat: Tree, testedBinder: Symbol): Result
        def eqTest(pat: Tree, testedBinder: Symbol): Result
        def and(a: Result, b: Result): Result
        def tru: Result
      }

      object treeCondStrategy extends TypeTestCondStrategy {
        type Result = Tree

        def and(a: Result, b: Result): Result                = a.select(defn.Boolean_&&).appliedTo(b)
        def tru                                              = Literal(Constant(true))
        def typeTest(testedBinder: Symbol, expectedTp: Type) = codegen._isInstanceOf(testedBinder, expectedTp)
        def nonNullTest(testedBinder: Symbol)                = ref(testedBinder).select(defn.Object_ne).appliedTo(Literal(Constant(null)))
        def equalsTest(pat: Tree, testedBinder: Symbol)      = codegen._equals(pat, testedBinder)
        def eqTest(pat: Tree, testedBinder: Symbol)          = ref(testedBinder).select(defn.Object_eq).appliedTo(pat)

        def outerTest(testedBinder: Symbol, expectedTp: Type): Tree = {
          val expectedOuter = expectedTp.normalizedPrefix match {
            //case NoType          => Literal(Constant(true)) // fallback for SI-6183 todo?
            case pre: SingletonType => singleton(pre)
          }

          // ExplicitOuter replaces `Select(q, outerSym) OBJ_EQ expectedPrefix` by `Select(q, outerAccessor(outerSym.owner)) OBJ_EQ expectedPrefix`
          // if there's an outer accessor, otherwise the condition becomes `true` -- TODO: can we improve needsOuterTest so there's always an outerAccessor?
          // val outer = expectedTp.typeSymbol.newMethod(vpmName.outer, newFlags = SYNTHETIC | ARTIFACT) setInfo expectedTp.prefix

          val expectedClass = expectedTp.dealias.classSymbol.asClass
          val test = codegen._asInstanceOf(testedBinder, expectedTp)
          // TODO: Use nme.OUTER_SELECT, like the Inliner does?
          val outerAccessorTested = ctx.atPhase(ctx.explicitOuterPhase.next) { implicit ctx =>
            ExplicitOuter.ensureOuterAccessors(expectedClass)
            test.select(ExplicitOuter.outerAccessor(expectedClass)).select(defn.Object_eq).appliedTo(expectedOuter)
          }
          outerAccessorTested
        }
      }

      /*object pureTypeTestChecker extends TypeTestCondStrategy {
        type Result = Boolean

        def typeTest(testedBinder: Symbol, expectedTp: Type): Result  = true

        def outerTest(testedBinder: Symbol, expectedTp: Type): Result = false
        def nonNullTest(testedBinder: Symbol): Result                 = false
        def equalsTest(pat: Tree, testedBinder: Symbol): Result       = false
        def eqTest(pat: Tree, testedBinder: Symbol): Result           = false
        def and(a: Result, b: Result): Result                         = false // we don't and type tests, so the conjunction must include at least one false
        def tru                                                       = true
      }*/

      def nonNullImpliedByTestChecker(binder: Symbol) = new TypeTestCondStrategy {
        type Result = Boolean

        def typeTest(testedBinder: Symbol, expectedTp: Type): Result  = testedBinder eq binder
        def outerTest(testedBinder: Symbol, expectedTp: Type): Result = false
        def nonNullTest(testedBinder: Symbol): Result                 = testedBinder eq binder
        def equalsTest(pat: Tree, testedBinder: Symbol): Result       = false // could in principle analyse pat and see if it's statically known to be non-null
        def eqTest(pat: Tree, testedBinder: Symbol): Result           = false // could in principle analyse pat and see if it's statically known to be non-null
        def and(a: Result, b: Result): Result                         = a || b
        def tru                                                       = false
      }
    }

    /** implements the run-time aspects of (§8.2) (typedPattern has already done the necessary type transformations)
      *
      * Type patterns consist of types, type variables, and wildcards. A type pattern T is of one of the following forms:
        - A reference to a class C, p.C, or T#C.
          This type pattern matches any non-null instance of the given class.
          Note that the prefix of the class, if it is given, is relevant for determining class instances.
          For instance, the pattern p.C matches only instances of classes C which were created with the path p as prefix.
          The bottom types scala.Nothing and scala.Null cannot be used as type patterns, because they would match nothing in any case.

        - A singleton type p.type.
          This type pattern matches only the value denoted by the path p
          (that is, a pattern match involved a comparison of the matched value with p using method eq in class AnyRef). // TODO: the actual pattern matcher uses ==, so that's what I'm using for now
          // https://issues.scala-lang.org/browse/SI-4577 "pattern matcher, still disappointing us at equality time"

        - A compound type pattern T1 with ... with Tn where each Ti is a type pat- tern.
          This type pattern matches all values that are matched by each of the type patterns Ti.

        - A parameterized type pattern T[a1,...,an], where the ai are type variable patterns or wildcards _.
          This type pattern matches all values which match T for some arbitrary instantiation of the type variables and wildcards.
          The bounds or alias type of these type variable are determined as described in (§8.3).

        - A parameterized type pattern scala.Array[T1], where T1 is a type pattern. // TODO
          This type pattern matches any non-null instance of type scala.Array[U1], where U1 is a type matched by T1.
      **/
    case class TypeTestTreeMaker(afterTest: Symbol, testedBinder: Symbol, expectedTp: Type, nextBinderTp: Type)(override val pos: Position, extractorArgTypeTest: Boolean = false) extends CondTreeMaker {
      import TypeTestTreeMaker._

      ctx.debuglog("TTTM" + ((prevBinder, extractorArgTypeTest, testedBinder, expectedTp, nextBinderTp)))

      val prevBinder = testedBinder

      val nextBinder = afterTest.asTerm

      def outerTestNeeded(implicit ctx: Context): Boolean = {
        // See the test for SI-7214 for motivation for dealias. Later `treeCondStrategy#outerTest`
        // generates an outer test based on `patType.prefix` with automatically dealises.
        expectedTp.dealias match {
          case tref @ TypeRef(pre: SingletonType, _) =>
            val s = tref
            s.symbol.isClass &&
            ExplicitOuter.needsOuterIfReferenced(s.symbol.asClass)
          case _ =>
            false
        }
      }

      override lazy val introducedRebindings = NoRebindings

      // the logic to generate the run-time test that follows from the fact that
      // a `prevBinder` is expected to have type `expectedTp`
      // the actual tree-generation logic is factored out, since the analyses generate Cond(ition)s rather than Trees
      // TODO: `null match { x : T }` will yield a check that (indirectly) tests whether `null ne null`
      // don't bother (so that we don't end up with the warning "comparing values of types Null and Null using `ne' will always yield false")
      def renderCondition(cs: TypeTestCondStrategy): cs.Result = {
        import cs._

        // propagate expected type
        def expTp(t: Tree): t.type = t // setType expectedTp todo:

        def testedWide              = testedBinder.info.widen
        def expectedWide            = expectedTp.widen
        def isAnyRef                = testedWide <:< defn.AnyRefType
        def isAsExpected            = testedWide <:< expectedTp
        def isExpectedPrimitiveType = isAsExpected && expectedTp.classSymbol.isPrimitiveValueClass
        def isExpectedReferenceType = isAsExpected && (expectedTp <:< defn.AnyRefType)
        def mkNullTest              = nonNullTest(testedBinder)
        def mkOuterTest             = outerTest(testedBinder, expectedTp)
        def mkTypeTest              = typeTest(testedBinder, expectedWide)

        def mkEqualsTest(lhs: Tree): cs.Result      = equalsTest(lhs, testedBinder)
        def mkEqTest(lhs: Tree): cs.Result          = eqTest(lhs, testedBinder)
        def addOuterTest(res: cs.Result): cs.Result = if (outerTestNeeded) and(res, mkOuterTest) else res

        // If we conform to expected primitive type:
        //   it cannot be null and cannot have an outer pointer. No further checking.
        // If we conform to expected reference type:
        //   have to test outer and non-null
        // If we do not conform to expected type:
        //   have to test type and outer (non-null is implied by successful type test)
        def mkDefault = (
          if (isExpectedPrimitiveType) tru
          else addOuterTest(
            if (isExpectedReferenceType) mkNullTest
            else mkTypeTest
          )
        )

        // true when called to type-test the argument to an extractor
        // don't do any fancy equality checking, just test the type
        // TODO: verify that we don't need to special-case Array
        // I think it's okay:
        //  - the isInstanceOf test includes a test for the element type
        //  - Scala's arrays are invariant (so we don't drop type tests unsoundly)
        if (extractorArgTypeTest) mkDefault
        else expectedTp match {
          case ThisType(tref) if tref.symbol.flags is Flags.Module            =>
            and(mkEqualsTest(ref(tref.symbol.companionModule)), mkTypeTest) // must use == to support e.g. List() == Nil
          case ConstantType(Constant(null)) if isAnyRef => mkEqTest(expTp(Literal(Constant(null))))
          case ConstantType(const)                      => mkEqualsTest(expTp(Literal(const)))
          case t: SingletonType                         => mkEqTest(singleton(expectedTp)) // SI-4577, SI-4897
          //case ThisType(sym)                            => mkEqTest(expTp(This(sym)))
          case _                                        => mkDefault
        }
      }

      val cond = renderCondition(treeCondStrategy)
      val res  = codegen._asInstanceOf(testedBinder, nextBinderTp)

      // is this purely a type test, e.g. no outer check, no equality tests (used in switch emission)
      //def isPureTypeTest = renderCondition(pureTypeTestChecker)

      def impliesBinderNonNull(binder: Symbol): Boolean =
      // @odersky: scalac is able to infer in this method that nonNullImpliedByTestChecker.Result,
      // dotty instead infers type projection TreeMakers.this.TypeTestTreeMaker.TypeTestCondStrategy#Result
      // which in turn doesn't typecheck in this method. Can you please explain why?
      // dotty deviation
        renderCondition(nonNullImpliedByTestChecker(binder)).asInstanceOf[Boolean]

      override def toString = "TT" + ((expectedTp, testedBinder.name, nextBinderTp))
    }

    // need to substitute to deal with existential types -- TODO: deal with existentials better, don't substitute (see RichClass during quick.comp)
    case class EqualityTestTreeMaker(prevBinder: Symbol, subpatBinder: Symbol, patTree: Tree, override val pos: Position) extends CondTreeMaker {
      val nextBinderTp = patTree.tpe & prevBinder.info
      val nextBinder = if (prevBinder eq subpatBinder) freshSym(pos, nextBinderTp) else subpatBinder

      // NOTE: generate `patTree == patBinder`, since the extractor must be in control of the equals method (also, patBinder may be null)
      // equals need not be well-behaved, so don't intersect with pattern's (stabilized) type (unlike MaybeBoundTyped's accumType, where it's required)
      val cond = codegen._equals(patTree, prevBinder)
      val res  = ref(prevBinder).ensureConforms(nextBinderTp)
      override def toString = "ET" + ((prevBinder.name, patTree))
    }

    case class AlternativesTreeMaker(prevBinder: Symbol, var altss: List[List[TreeMaker]], pos: Position) extends TreeMaker with NoNewBinders {
      // don't substitute prevBinder to nextBinder, a set of alternatives does not need to introduce a new binder, simply reuse the previous one

      override private[TreeMakers] def incorporateOuterRebinding(outerSubst: Rebindings): Unit = {
        super.incorporateOuterRebinding(outerSubst)
        altss = altss map (alts => propagateRebindings(alts, rebindings))
      }

      def chainBefore(next: Tree)(codegenAlt: Casegen): Tree = {
        /*atPos(pos)*/{
          // one alternative may still generate multiple trees (e.g., an extractor call + equality test)
          // (for now,) alternatives may not bind variables (except wildcards), so we don't care about the final substitution built internally by makeTreeMakers
          val combinedAlts = altss map (altTreeMakers =>
            ((casegen: Casegen) => combineExtractors(altTreeMakers :+ TrivialTreeMaker(casegen.one(Literal(Constant(true)))))(casegen))
            )

          val findAltMatcher = codegenAlt.matcher(EmptyTree, NoSymbol, defn.BooleanType)(combinedAlts, Some((x: Symbol) => Literal(Constant(false))))
          codegenAlt.ifThenElseZero(findAltMatcher, next)
        }
      }
    }

    case class GuardTreeMaker(guardTree: Tree) extends TreeMaker with NoNewBinders {
      val pos = guardTree.pos

      def chainBefore(next: Tree)(casegen: Casegen): Tree = casegen.flatMapGuard(guardTree, next)
      override def toString = "G(" + guardTree + ")"
    }

    // combineExtractors changes the current substitution's of the tree makers in `treeMakers`
    // requires propagateSubstitution(treeMakers) has been called
    def combineExtractors(treeMakers: List[TreeMaker])(casegen: Casegen): Tree = {
      val (testsMakers, guardAndBodyMakers) = treeMakers.span(t => !(t.isInstanceOf[NoNewBinders]))
      val body = guardAndBodyMakers.foldRight(EmptyTree: Tree)((a, b) => a.chainBefore(b)(casegen))
      val rebindings = guardAndBodyMakers.last.rebindings.emitValDefs
      testsMakers.foldRight(Block(rebindings, body): Tree)((a, b) => a.chainBefore(b)(casegen))
    }
    // a foldLeft to accumulate the localSubstitution left-to-right
    // unlike in scalace it does not drop SubstOnly tree makers,
    // as there could be types having them as prefix
    def propagateRebindings(treeMakers: List[TreeMaker], initial: Rebindings): List[TreeMaker] = {
      var accumSubst: Rebindings = initial
      treeMakers foreach { maker =>
        maker incorporateOuterRebinding accumSubst
        accumSubst = maker.rebindings
      }
      treeMakers
    }

    // calls propagateSubstitution on the treemakers
    def combineCases(scrut: Tree, scrutSym: Symbol, casesRaw: List[List[TreeMaker]], pt: Type, owner: Symbol, matchFailGenOverride: Option[Symbol => Tree]): Tree = {
      // unlike in scalac SubstOnlyTreeMakers are maintained.
      val casesRebindingPropagated = casesRaw map (propagateRebindings(_, NoRebindings))

      def matchFailGen = matchFailGenOverride orElse Some((arg: Symbol) => Throw(New(defn.MatchErrorType, List(ref(arg)))))

      ctx.debuglog("combining cases: " + (casesRebindingPropagated.map(_.mkString(" >> ")).mkString("{", "\n", "}")))

        val (suppression, requireSwitch): (Suppression, Boolean) =
          /*if (settings.XnoPatmatAnalysis)*/ (Suppression.NoSuppression, false)
          /*else scrut match {
            case Typed(tree, tpt) =>
              val suppressExhaustive = tpt.tpe hasAnnotation UncheckedClass
              val supressUnreachable = tree match {
                case Ident(name) if name startsWith nme.CHECK_IF_REFUTABLE_STRING => true // SI-7183 don't warn for withFilter's that turn out to be irrefutable.
                case _ => false
              }
              val suppression = Suppression(suppressExhaustive, supressUnreachable)
              // matches with two or fewer cases need not apply for switchiness (if-then-else will do)
              val requireSwitch = treeInfo.isSwitchAnnotation(tpt.tpe) && casesNoSubstOnly.lengthCompare(2) > 0
              (suppression, requireSwitch)
            case _ =>
              (Suppression.NoSuppression, false)
          }*/

        emitSwitch(scrut, scrutSym, casesRebindingPropagated, pt, matchFailGenOverride, suppression.exhaustive).getOrElse{
          if (requireSwitch) ctx.warning("could not emit switch for @switch annotated match", scrut.pos)

          if (casesRebindingPropagated nonEmpty) {
            // before optimizing, check casesNoSubstOnly for presence of a default case,
            // since DCE will eliminate trivial cases like `case _ =>`, even if they're the last one
            // exhaustivity and reachability must be checked before optimization as well
            // TODO: improve notion of trivial/irrefutable -- a trivial type test before the body still makes for a default case
            //   ("trivial" depends on whether we're emitting a straight match or an exception, or more generally, any supertype of scrutSym.tpe is a no-op)
            //   irrefutability checking should use the approximation framework also used for CSE, unreachability and exhaustivity checking
            val synthCatchAll: Option[Symbol => Tree] =
              if (casesRebindingPropagated.nonEmpty && {
                val nonTrivLast = casesRebindingPropagated.last
                nonTrivLast.nonEmpty && nonTrivLast.head.isInstanceOf[BodyTreeMaker]
              }) None
              else matchFailGen

            analyzeCases(scrutSym, casesRebindingPropagated, pt, suppression)

            val (cases, toHoist) = optimizeCases(scrutSym, casesRebindingPropagated, pt)

            val matchRes = codegen.matcher(scrut, scrutSym, pt)(cases.map(x => combineExtractors(x) _), synthCatchAll)

            if (toHoist isEmpty) matchRes else Block(toHoist, matchRes)
          } else {
            codegen.matcher(scrut, scrutSym, pt)(Nil, matchFailGen)
          }
        }
      }
  }

  trait MatchOptimizer extends OptimizedCodegen with TreeMakers
  /*with SwitchEmission // todo: toBe ported
  with CommonSubconditionElimination*/ {
    override def optimizeCases(prevBinder: Symbol, cases: List[List[TreeMaker]], pt: Type): (List[List[TreeMaker]], List[Tree]) = {
      // TODO: do CSE on result of doDCE(prevBinder, cases, pt)
      val optCases = cases// todo: doCSE(prevBinder, cases, pt)
      val toHoist = Nil/*(
        for (treeMakers <- optCases)
        yield treeMakers.collect{case tm: ReusedCondTreeMaker => tm.treesToHoist}
        ).flatten.flatten.toList*/
      (optCases, toHoist)
    }
  }

  trait MatchTranslator extends TreeMakers with ScalacPatternExpanders {

    def isVarPattern(pat: Tree): Boolean = pat match {
      case x: BackquotedIdent => false
      case x: Ident => x.name.isVariableName
      case _ => false
    }

    /** A conservative approximation of which patterns do not discern anything.
      * They are discarded during the translation.
      */
    object WildcardPattern {
      def unapply(pat: Tree): Boolean = pat match {
        case Typed(_, arg) if arg.tpe.isRepeatedParam => true
        case Bind(nme.WILDCARD, WildcardPattern()) => true // don't skip when binding an interesting symbol!
        case t if (tpd.isWildcardArg(t))           => true
        case x: Ident                              => isVarPattern(x)
        case Alternative(ps)                       => ps forall unapply
        case EmptyTree                             => true
        case _                                     => false
      }
    }

    object PatternBoundToUnderscore {
      def unapply(pat: Tree): Boolean = pat match {
        case Bind(nme.WILDCARD, _)                => true // don't skip when binding an interesting symbol!
        case Ident(nme.WILDCARD)                  => true
        case Alternative(ps)                      => ps forall unapply
        case Typed(PatternBoundToUnderscore(), _) => false // true // Dmitry: change in dotty. Type test will be performed and the field must be stored
        case _                                    => false
      }
    }

    object SymbolBound {
      def unapply(tree: Tree): Option[(Symbol, Tree)] = tree match {
        case Bind(_, expr) if tree.symbol.exists => Some(tree.symbol -> expr)
        case _                                   => None
      }
    }

    def newBoundTree(tree: Tree, pt: Type): BoundTree = tree match {
      case SymbolBound(sym, Typed(subpat, tpe)) => BoundTree(freshSym(tree.pos, pt, PatMatPiName), tree)
      case SymbolBound(sym, expr) => BoundTree(sym, expr)
      case _                      => BoundTree(freshSym(tree.pos, pt, PatMatPName), tree)
    }

    /*final*/ case class BoundTree(binder: Symbol, tree: Tree) {
      private lazy val extractor = ExtractorCall(tree, binder)

      def pos = tree.pos
      def tpe = binder.info.widenDealias
      def pt  = unbound match {
        // case Star(tpt)      => this glbWith seqType(tpt.tpe) dd todo:
        case TypeBound(tpe) => tpe
        case tree           => tree.tpe
      }

      def glbWith(other: Type) = ctx.typeComparer.glb(tpe :: other :: Nil)// .normalize

      object SymbolAndTypeBound {
        def unapply(tree: Tree): Option[(Symbol, Type)] = tree match {
          case SymbolBound(sym, Typed(_: UnApply, _)) => None // see comment in #189
          case SymbolBound(sym, TypeBound(tpe)) => Some(sym -> tpe)
          case TypeBound(tpe)                   => Some(binder -> tpe)
          case _                                => None
        }
      }

      object SymbolAndValueBound {
        def unapply(tree: Tree): Option[(Symbol, Tree)] = tree match {
          case SymbolBound(sym, ConstantPattern(const)) => Some(sym -> const)
          case _                                => None
        }
      }

      object TypeBound {
        def unapply(tree: Tree): Option[Type] = tree match {
          case Typed(_, arg) if !arg.tpe.isRepeatedParam => Some(tree.typeOpt)
          case _                                         => None
        }
      }

      object ConstantPattern {
        def unapply(tree: Tree): Option[Tree] = tree match {
          case Literal(Constant(_)) | Ident(_) | Select(_, _) | This(_) => Some(tree)
          case _ => None
        }
      }

      private def rebindTo(pattern: Tree) = BoundTree(binder, pattern)
      private def step(treeMakers: TreeMaker*)(subpatterns: BoundTree*): TranslationStep = TranslationStep(treeMakers.toList, subpatterns.toList)

      private def bindingStep(sub: Symbol, subpattern: Tree) = step(SubstOnlyTreeMaker(sub, binder))(rebindTo(subpattern))
      private def equalityTestStep(testedSymbol: Symbol, constantSymbol: Symbol, constant: Tree)
                                                             = step(EqualityTestTreeMaker(testedSymbol, constantSymbol, constant, pos))()
      private def typeTestStep(sub: Symbol, subPt: Type)     = step(TypeTestTreeMaker(sub, binder, subPt, sub.termRef)(pos))()
      private def alternativesStep(alts: List[Tree])         = step(AlternativesTreeMaker(binder, translatedAlts(alts), alts.head.pos))()
      private def translatedAlts(alts: List[Tree])           = alts map (alt => rebindTo(alt).translate())
      private def noStep()                                   = step()()

      private def unsupportedPatternMsg =
        i"unsupported pattern: ${tree.show} / $this (this is a scalac bug.)"

      // example check: List[Int] <:< ::[Int]
      private def extractorStep(): TranslationStep = {
        def paramType = extractor.aligner.wholeType
        import extractor.treeMaker
        // chain a type-testing extractor before the actual extractor call
        // it tests the type, checks the outer pointer and casts to the expected type
        // TODO: the outer check is mandated by the spec for case classes, but we do it for user-defined unapplies as well [SPEC]
        // (the prefix of the argument passed to the unapply must equal the prefix of the type of the binder)
        lazy val typeTest = TypeTestTreeMaker(freshSym(pos, paramType), binder, paramType, paramType)(pos, extractorArgTypeTest = true)
        // check whether typetest implies binder is not null,
        // even though the eventual null check will be on typeTest.nextBinder
        // it'll be equal to binder casted to paramType anyway (and the type test is on binder)
        def extraction: TreeMaker = treeMaker(typeTest.nextBinder, typeTest.impliesBinderNonNull(binder), pos, paramType)

        // paramType = the type expected by the unapply
        // TODO: paramType may contain unbound type params (run/t2800, run/t3530)
        val makers = (
          // Statically conforms to paramType
          if (tpe <:< paramType) treeMaker(binder, false, pos, tpe) :: Nil
          else typeTest :: extraction :: Nil
        )
        step(makers: _*)(extractor.subBoundTrees: _*)
      }

      // Summary of translation cases. I moved the excerpts from the specification further below so all
      // the logic can be seen at once.
      //
      // [1] skip wildcard trees -- no point in checking them
      // [2] extractor and constructor patterns
      // [3] replace subpatBinder by patBinder, as if the Bind was not there.
      //     It must be patBinder, as subpatBinder has the wrong info: even if the bind assumes a better type,
      //     this is not guaranteed until we cast
      // [4] typed patterns - a typed pattern never has any subtrees
      //     must treat Typed and Bind together -- we need to know the patBinder of the Bind pattern to get at the actual type
      // [5] literal and stable id patterns
      // [6] pattern alternatives
      // [7] symbol-less bind patterns - this happens in certain ill-formed programs, there'll be an error later
      //     don't fail here though (or should we?)
      def nextStep(): TranslationStep = tree match {
        case _: UnApply | _: Apply | Typed(_: UnApply | _: Apply, _)  => extractorStep()
        case SymbolAndTypeBound(sym, tpe)                             => typeTestStep(sym, tpe)
        case TypeBound(tpe)                                           => typeTestStep(binder, tpe)
        case SymbolBound(sym, expr)                                   => bindingStep(sym, expr)
        case WildcardPattern()                                        => noStep()
        case ConstantPattern(const)                                   => equalityTestStep(binder, binder, const)
        case Alternative(alts)                                        => alternativesStep(alts)
        case _                                                        => ctx.error(unsupportedPatternMsg, pos) ; noStep()
      }
      def translate(): List[TreeMaker] = nextStep() merge (_.translate())

      private def concreteType = tpe.bounds.hi
      private def unbound = unbind(tree)
      private def tpe_s = if (pt <:< concreteType) "" + pt else s"$pt (binder: $tpe)"
      private def at_s = unbound match {
        case WildcardPattern() => ""
        case pat               => s" @ $pat"
      }
      override def toString = s"${binder.name}: $tpe_s$at_s"
    }

    // a list of TreeMakers that encode `patTree`, and a list of arguments for recursive invocations of `translatePattern` to encode its subpatterns
    /*final*/ case class TranslationStep(makers: List[TreeMaker], subpatterns: List[BoundTree]) {
      def merge(f: BoundTree => List[TreeMaker]): List[TreeMaker] = makers ::: (subpatterns flatMap f)
      override def toString = if (subpatterns.isEmpty) "" else subpatterns.mkString("(", ", ", ")")
    }

    def isSyntheticDefaultCase(cdef: CaseDef) = cdef match {
      case CaseDef(Bind(nme.DEFAULT_CASE, _), EmptyTree, _) => true
      case _                                                => false
    }

    /** Implement a pattern match by turning its cases (including the implicit failure case)
      * into the corresponding (monadic) extractors, and combining them with the `orElse` combinator.
      *
      * For `scrutinee match { case1 ... caseN }`, the resulting tree has the shape
      * `runOrElse(scrutinee)(x => translateCase1(x).orElse(translateCase2(x)).....orElse(zero))`
      *
      * NOTE: the resulting tree is not type checked, nor are nested pattern matches transformed
      *   thus, you must typecheck the result (and that will in turn translate nested matches)
      *   this could probably be optimized... (but note that the matchStrategy must be solved for each nested patternmatch)
      */
    def translateMatch(match_ : Match): Tree = {
      val Match(sel, cases) = match_

      val selectorTp = sel.tpe.widen/*withoutAnnotations*/

      val selectorSym = freshSym(sel.pos, selectorTp, PatMatSelectorName)

      val (nonSyntheticCases, defaultOverride) = cases match {
        case init :+ last if isSyntheticDefaultCase(last) => (init, Some(((scrut: Symbol) => last.body)))
        case _                                            => (cases, None)
      }


      // checkMatchVariablePatterns(nonSyntheticCases) // only used for warnings

      // we don't transform after uncurry
      // (that would require more sophistication when generating trees,
      //  and the only place that emits Matches after typers is for exception handling anyway)
      /*if (phase.id >= currentRun.uncurryPhase.id)
        devWarning(s"running translateMatch past uncurry (at $phase) on $selector match $cases")*/

      ctx.debuglog("translating " + cases.mkString("{", "\n", "}"))

      //val start = if (Statistics.canEnable) Statistics.startTimer(patmatNanos) else null

      // when one of the internal cps-type-state annotations is present, strip all CPS annotations
      ///val origPt  = removeCPSFromPt(match_.tpe)
      // relevant test cases: pos/existentials-harmful.scala, pos/gadt-gilles.scala, pos/t2683.scala, pos/virtpatmat_exist4.scala
      // pt is the skolemized version
      val pt = match_.tpe.widen //repeatedToSeq(origPt)

      // val packedPt = repeatedToSeq(typer.packedType(match_, context.owner))
      selectorSym.setFlag(Flags.Synthetic | Flags.Case)

      // pt = Any* occurs when compiling test/files/pos/annotDepMethType.scala  with -Xexperimental
      val combined = combineCases(sel, selectorSym, nonSyntheticCases map translateCase(selectorSym, pt), pt, ctx.owner, defaultOverride)

      // if (Statistics.canEnable) Statistics.stopTimer(patmatNanos, start)
      Block(List(ValDef(selectorSym, sel)), combined)
    }

    /**  The translation of `pat if guard => body` has two aspects:
      *     1) the substitution due to the variables bound by patterns
      *     2) the combination of the extractor calls using `flatMap`.
      *
      * 2) is easy -- it looks like: `translatePattern_1.flatMap(translatePattern_2....flatMap(translatePattern_N.flatMap(translateGuard.flatMap((x_i) => success(Xbody(x_i)))))...)`
      *     this must be right-leaning tree, as can be seen intuitively by considering the scope of bound variables:
      *     variables bound by pat_1 must be visible from the function inside the left-most flatMap right up to Xbody all the way on the right
      * 1) is tricky because translatePattern_i determines the shape of translatePattern_i + 1:
      *    zoom in on `translatePattern_1.flatMap(translatePattern_2)` for example -- it actually looks more like:
      *      `translatePattern_1(x_scrut).flatMap((x_1) => {y_i -> x_1._i}translatePattern_2)`
      *
      *    `x_1` references the result (inside the monad) of the extractor corresponding to `pat_1`,
      *    this result holds the values for the constructor arguments, which translatePattern_1 has extracted
      *    from the object pointed to by `x_scrut`. The `y_i` are the symbols bound by `pat_1` (in order)
      *    in the scope of the remainder of the pattern, and they must thus be replaced by:
      *      - (for 1-ary unapply) x_1
      *      - (for n-ary unapply, n > 1) selection of the i'th tuple component of `x_1`
      *      - (for unapplySeq) x_1.apply(i)
      *
      *    in the treemakers,
      *
      *    Thus, the result type of `translatePattern_i`'s extractor must conform to `M[(T_1,..., T_n)]`.
      *
      *    Operationally, phase 1) is a foldLeft, since we must consider the depth-first-flattening of
      *    the transformed patterns from left to right. For every pattern ast node, it produces a transformed ast and
      *    a function that will take care of binding and substitution of the next ast (to the right).
      *
      */
    def translateCase(scrutSym: Symbol, pt: Type)(caseDef: CaseDef): List[TreeMaker] = {
      val CaseDef(pattern, guard, body) = caseDef
      translatePattern(BoundTree(scrutSym, pattern)) ++ translateGuard(guard) :+ translateBody(body, pt)
    }

    def translatePattern(bound: BoundTree): List[TreeMaker] = bound.translate()

    def translateGuard(guard: Tree): List[TreeMaker] =
      if (guard == EmptyTree) Nil
      else List(GuardTreeMaker(guard))

    // TODO: 1) if we want to support a generalisation of Kotlin's patmat continue, must not hard-wire lifting into the monad (which is now done by codegen.one),
    // so that user can generate failure when needed -- use implicit conversion to lift into monad on-demand?
    // to enable this, probably need to move away from Option to a monad specific to pattern-match,
    // so that we can return Option's from a match without ambiguity whether this indicates failure in the monad, or just some result in the monad
    // 2) body.tpe is the type of the body after applying the substitution that represents the solution of GADT type inference
    // need the explicit cast in case our substitutions in the body change the type to something that doesn't take GADT typing into account
    def translateBody(body: Tree, matchPt: Type): TreeMaker =
      BodyTreeMaker(body, matchPt)

    // Some notes from the specification

    /*A constructor pattern is of the form c(p1, ..., pn) where n ≥ 0.
      It consists of a stable identifier c, followed by element patterns p1, ..., pn.
      The constructor c is a simple or qualified name which denotes a case class (§5.3.2).

      If the case class is monomorphic, then it must conform to the expected type of the pattern,
      and the formal parameter types of x’s primary constructor (§5.3) are taken as the expected
      types of the element patterns p1, ..., pn.

      If the case class is polymorphic, then its type parameters are instantiated so that the
      instantiation of c conforms to the expected type of the pattern.
      The instantiated formal parameter types of c’s primary constructor are then taken as the
      expected types of the component patterns p1, ..., pn.

      The pattern matches all objects created from constructor invocations c(v1, ..., vn)
      where each element pattern pi matches the corresponding value vi .
      A special case arises when c’s formal parameter types end in a repeated parameter.
      This is further discussed in (§8.1.9).
    **/

    /* A typed pattern x : T consists of a pattern variable x and a type pattern T.
       The type of x is the type pattern T, where each type variable and wildcard is replaced by a fresh, unknown type.
       This pattern matches any value matched by the type pattern T (§8.2); it binds the variable name to that value.
    */

    /* A pattern binder x@p consists of a pattern variable x and a pattern p.
       The type of the variable x is the static type T of the pattern p.
       This pattern matches any value v matched by the pattern p,
       provided the run-time type of v is also an instance of T,  <-- TODO! https://issues.scala-lang.org/browse/SI-1503
       and it binds the variable name to that value.
    */

    /* 8.1.4 Literal Patterns
         A literal pattern L matches any value that is equal (in terms of ==) to the literal L.
         The type of L must conform to the expected type of the pattern.

       8.1.5 Stable Identifier Patterns  (a stable identifier r (see §3.1))
         The pattern matches any value v such that r == v (§12.1).
         The type of r must conform to the expected type of the pattern.
    */


    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // helper methods: they analyze types and trees in isolation, but they are not (directly) concerned with the structure of the overall translation
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    object ExtractorCall {
      // TODO: check unargs == args
      def apply(tree: Tree, binder: Symbol): ExtractorCall = {
        tree match {
          case Typed(unapply, _) => apply(unapply, binder)
          case UnApply(unfun, implicits, args) =>
            val mt @ MethodType(_) = unfun.tpe.widen
            val castedBinder = ref(binder).ensureConforms(mt.paramInfos.head)
            var synth = unfun.appliedTo(castedBinder)
            if (implicits.nonEmpty) synth = synth.appliedToArgs(implicits)
            new ExtractorCallRegular(alignPatterns(tree, synth.tpe), synth, args, synth.tpe)
        }
      }
    }

    abstract class ExtractorCall(val aligner: PatternAligned) {

      import aligner._

      def args: List[Tree]

      // don't go looking for selectors if we only expect one pattern
      def rawSubPatTypes = aligner.extractedTypes

      def typeArgOfBaseTypeOr(tp: Type, baseClass: Symbol)(or: => Type): Type = (tp.baseType(baseClass)).argInfos match {
        case x :: Nil => x
        case _        => or
      }

      def resultInMonad  =
        if (aligner.isBool) defn.UnitType
        else if (isProductMatch(resultType, aligner.prodArity)) resultType
        else if (isGetMatch(resultType)) extractorMemberType(resultType, nme.get)
        else resultType

      def resultType: Type

      /** Create the TreeMaker that embodies this extractor call
        *
        * `binder` has been casted to `paramType` if necessary
        * `binderKnownNonNull` indicates whether the cast implies `binder` cannot be null
        * when `binderKnownNonNull` is `true`, `ProductExtractorTreeMaker` does not do a (redundant) null check on binder
        */
      def treeMaker(binder: Symbol, binderKnownNonNull: Boolean, pos: Position, binderTypeTested: Type): TreeMaker

      // `subPatBinders` are the variables bound by this pattern in the following patterns
      // subPatBinders are replaced by references to the relevant part of the extractor's result (tuple component, seq element, the result as-is)
      // must set infos to `subPatTypes`, which are provided by extractor's result,
      // as b.info may be based on a Typed type ascription, which has not been taken into account yet by the translation
      // (it will later result in a type test when `tp` is not a subtype of `b.info`)
      // TODO: can we simplify this, together with the Bound case?
      def subPatBinders = subBoundTrees map (_.binder)
      lazy val subBoundTrees = (args, subPatTypes).zipped map newBoundTree

      // never store these in local variables (for PreserveSubPatBinders)
      lazy val ignoredSubPatBinders: Set[Symbol] = subPatBinders zip args collect { case (b, PatternBoundToUnderscore()) => b } toSet

      // do repeated-parameter expansion to match up with the expected number of arguments (in casu, subpatterns)
      private def nonStarSubPatTypes = aligner.typedNonStarPatterns map (_.tpe)

      def subPatTypes: List[Type] = typedPatterns map (_.tpe)

      // there are `prodArity` non-seq elements in the tuple.
      protected def firstIndexingBinder = prodArity
      protected def expectedLength      = elementArity
      protected def lastIndexingBinder  = totalArity - starArity - 1

      private def productElemsToN(binder: Symbol, n: Int): List[Tree] = 1 to n map tupleSel(binder) toList
      private def genTake(binder: Symbol, n: Int): List[Tree]         = (0 until n).toList map (codegen index seqTree(binder))
      private def genDrop(binder: Symbol, n: Int): List[Tree]         = codegen.drop(seqTree(binder))(expectedLength) :: Nil

      // codegen.drop(seqTree(binder))(nbIndexingIndices)))).toList
      protected def seqTree(binder: Symbol)                = tupleSel(binder)(firstIndexingBinder + 1)
      protected def tupleSel(binder: Symbol)(i: Int): Tree = {
        val accessors =
          if (defn.isProductSubType(binder.info))
            productSelectors(binder.info)
          else binder.caseAccessors
        val res =
          if (accessors.isDefinedAt(i - 1)) ref(binder).select(accessors(i - 1).name)
          else codegen.tupleSel(binder)(i) // this won't type check for case classes, as they do not inherit ProductN
        val rsym = res.symbol // just for debugging
        res
      }

      // the trees that select the subpatterns on the extractor's result,
      // referenced by `binder`
      protected def subPatRefsSeq(binder: Symbol): List[Tree] = {
        def lastTrees: List[Tree] = (
          if (!aligner.isStar) Nil
          else if (expectedLength == 0) seqTree(binder) :: Nil
          else genDrop(binder, expectedLength)
        )
        // this error-condition has already been checked by checkStarPatOK:
        //   if (isSeq) assert(firstIndexingBinder + nbIndexingIndices + (if (lastIsStar) 1 else 0) == totalArity, "(resultInMonad, ts, subPatTypes, subPats)= " +(resultInMonad, ts, subPatTypes, subPats))

        // [1] there are `firstIndexingBinder` non-seq tuple elements preceding the Seq
        // [2] then we have to index the binder that represents the sequence for the remaining subpatterns, except for...
        // [3] the last one -- if the last subpattern is a sequence wildcard:
        //       drop the prefix (indexed by the refs on the preceding line), return the remainder
        (    productElemsToN(binder, firstIndexingBinder)
          ++ genTake(binder, expectedLength)
          ++ lastTrees
        ).toList
      }

      // the trees that select the subpatterns on the extractor's result, referenced by `binder`
      // require (nbSubPats > 0 && (!lastIsStar || isSeq))
      protected def subPatRefs(binder: Symbol): List[Tree] = {
        val refs = if (totalArity > 0 && isSeq) subPatRefsSeq(binder)
        else if (binder.info.member(nme._1).exists && !isSeq) productElemsToN(binder, totalArity)
        else ref(binder) :: Nil
        refs
      }

      val mathSignymSymbol = defn.ScalaMathPackageVal.requiredMethod("signum".toTermName, List(defn.IntType))
      val mathSignum = ref(defn.ScalaMathPackageVal).select(mathSignymSymbol)


      private def compareInts(t1: Tree, t2: Tree) =
        mathSignum.appliedTo(t1.select(defn.Int_-).appliedTo(t2))
        //gen.mkMethodCall(termMember(ScalaPackage, "math"), TermName("signum"), Nil, (t1 INT_- t2) :: Nil)

      protected def lengthGuard(binder: Symbol): Option[Tree] =
      // no need to check unless it's an unapplySeq and the minimal length is non-trivially satisfied
        checkedLength map { expectedLength =>
          // `binder.lengthCompare(expectedLength)`
          // ...if binder has a lengthCompare method, otherwise
          // `scala.math.signum(binder.length - expectedLength)`
          def checkExpectedLength: Tree = sequenceType.member(nme.lengthCompare) match {
            case NoDenotation => compareInts(Select(seqTree(binder), nme.length), Literal(Constant(expectedLength)))
            case x:SingleDenotation   => (seqTree(binder).select(x.symbol)).appliedTo(Literal(Constant(expectedLength)))
            case _ =>
              ctx.error("TODO: multiple lengthCompare")
              EmptyTree
          }

          // the comparison to perform
          // when the last subpattern is a wildcard-star the expectedLength is but a lower bound
          // (otherwise equality is required)
          def compareOp: (Tree, Tree) => Tree =
            if (aligner.isStar) _.select(defn.Int_>=).appliedTo(_)
            else _.select(defn.Int_==).appliedTo(_)

          // `if (binder != null && $checkExpectedLength [== | >=] 0) then else zero`
          (seqTree(binder).select(defn.Any_!=).appliedTo(Literal(Constant(null)))).select(defn.Boolean_&&).appliedTo(compareOp(checkExpectedLength, Literal(Constant(0))))
        }

      def checkedLength: Option[Int] =
      // no need to check unless it's an unapplySeq and the minimal length is non-trivially satisfied
        if (!isSeq || expectedLength < starArity) None
        else Some(expectedLength)
    }

    class ExtractorCallRegular(aligner: PatternAligned, extractorCallIncludingDummy: Tree, val args: List[Tree], val resultType: Type) extends ExtractorCall(aligner) {

      /** Create the TreeMaker that embodies this extractor call
        *
        *  `binder` has been casted to `paramType` if necessary
        *  `binderKnownNonNull` is not used in this subclass
        *
        *  TODO: implement review feedback by @retronym:
        *    Passing the pair of values around suggests:
        *       case class Binder(sym: Symbol, knownNotNull: Boolean).
        *    Perhaps it hasn't reached critical mass, but it would already clean things up a touch.
        */
      def treeMaker(patBinderOrCasted: Symbol, binderKnownNonNull: Boolean, pos: Position, binderTypeTested: Type): TreeMaker = {
        // the extractor call (applied to the binder bound by the flatMap corresponding
        // to the previous (i.e., enclosing/outer) pattern)
        val extractorApply = extractorCallIncludingDummy// spliceApply(patBinderOrCasted)
        // can't simplify this when subPatBinders.isEmpty, since UnitTpe is definitely
        // wrong when isSeq, and resultInMonad should always be correct since it comes
        // directly from the extractor's result type
        val binder = freshSym(pos, resultInMonad)
        val spb = subPatBinders
        ExtractorTreeMaker(extractorApply, lengthGuard(binder), binder)(
          spb,
          subPatRefs(binder, spb, resultType),
          aligner.isBool,
          checkedLength,
          patBinderOrCasted,
          ignoredSubPatBinders
        )
      }

      override protected def seqTree(binder: Symbol): Tree =
        if (firstIndexingBinder == 0) ref(binder)
        else super.seqTree(binder)

      // the trees that select the subpatterns on the extractor's result, referenced by `binder`
      // require (totalArity > 0 && (!lastIsStar || isSeq))
      protected def subPatRefs(binder: Symbol, subpatBinders: List[Symbol], binderTypeTested: Type): List[Tree] = {
        if (aligner.isSingle && aligner.extractor.prodArity == 1 && defn.isTupleType(binder.info)) {
          // special case for extractor
          // comparing with scalac additional assertions added
          val subpw = subpatBinders.head.info.widen
          val binderw = binder.info.widen
          val go = subpatBinders.head.info <:< binder.info
          val go1 = binder.info <:< subpatBinders.head.info
          //val spr = subPatRefs(binder)
          assert(go && go1)
          ref(binder) :: Nil
        }
        else if ((aligner.isSingle && aligner.extractor.prodArity == 1) &&
                 !isProductMatch(binderTypeTested, aligner.prodArity) && isGetMatch(binderTypeTested))
          List(ref(binder))
        else
          subPatRefs(binder)
      }

      /*protected def spliceApply(binder: Symbol): Tree = {
        object splice extends TreeMap {
          def binderRef(pos: Position): Tree =
            ref(binder) //setPos pos

          override def transform(t: tpd.Tree)(implicit ctx: Context): tpd.Tree = t match {
            // duplicated with the extractor Unapplied
            case Apply(x, List(i @ Ident(nme.SELECTOR_DUMMY))) =>
              cpy.Apply(t, x, binderRef(i.pos) :: Nil)
            // SI-7868 Account for numeric widening, e.g. <unappplySelector>.toInt
            case Apply(x, List(i @ (sel @ Select(Ident(nme.SELECTOR_DUMMY), name)))) =>
              cpy.Apply(t, x, cpy.Select(sel, binderRef(i.pos), name) :: Nil)
            case _ =>
              super.transform(t)
          }
        }
        splice transform extractorCallIncludingDummy
      }*/

      override def rawSubPatTypes = aligner.extractor.varargsTypes
    }
  }

  /** An extractor returns: F1, F2, ..., Fi, opt[Seq[E] or E*]
    *        A case matches: P1, P2, ..., Pj, opt[Seq[E]]
    *          Put together: P1/F1, P2/F2, ... Pi/Fi, Pi+1/E, Pi+2/E, ... Pj/E, opt[Seq[E]]
    *
    *  Here Pm/Fi is the last pattern to match the fixed arity section.
    *
    *    prodArity: the value of i, i.e. the number of non-sequence types in the extractor
    *    nonStarArity: the value of j, i.e. the number of non-star patterns in the case definition
    *    elementArity: j - i, i.e. the number of non-star patterns which must match sequence elements
    *       starArity: 1 or 0 based on whether there is a star (sequence-absorbing) pattern
    *      totalArity: nonStarArity + starArity, i.e. the number of patterns in the case definition
    *
    *  Note that prodArity is a function only of the extractor, and
    *  nonStar/star/totalArity are all functions of the patterns. The key
    *  value for aligning and typing the patterns is elementArity, as it
    *  is derived from both sets of information.
    */
  trait PatternExpander[Pattern, Type] {
    /** You'll note we're not inside the cake. "Pattern" and "Type" are
      *  arbitrary types here, and NoPattern and NoType arbitrary values.
      */
    def NoPattern: Pattern
    def NoType: Type

    /** It's not optimal that we're carrying both sequence and repeated
      *  type here, but the implementation requires more unraveling before
      *  it can be avoided.
      *
      *  sequenceType is Seq[T], elementType is T, repeatedType is T*.
      */
    sealed case class Repeated(sequenceType: Type, elementType: Type, repeatedType: Type) {
      def exists = elementType != NoType

      def elementList  = if (exists) elementType :: Nil else Nil
      def sequenceList = if (exists) sequenceType :: Nil else Nil
      def repeatedList = if (exists) repeatedType :: Nil else Nil

      override def toString = s"${elementType}*"
    }
    object NoRepeated extends Repeated(NoType, NoType, NoType) {
      override def toString = "<none>"
    }

    /*final*/ case class Patterns(fixed: List[Pattern], star: Pattern) {
      def hasStar      = star != NoPattern
      def starArity    = if (hasStar) 1 else 0
      def nonStarArity = fixed.length
      def totalArity   = nonStarArity + starArity
      def starPatterns = if (hasStar) star :: Nil else Nil
      def all          = fixed ::: starPatterns

      override def toString = all mkString ", "
    }

    /** An 'extractor' can be a case class or an unapply or unapplySeq method.
      *  Decoding what it is that they extract takes place before we arrive here,
      *  so that this class can concentrate only on the relationship between
      *  patterns and types.
      *
      *  In a case class, the class is the unextracted type and the fixed and
      *  repeated types are derived from its constructor parameters.
      *
      *  In an unapply, this is reversed: the parameter to the unapply is the
      *  unextracted type, and the other types are derived based on the return
      *  type of the unapply method.
      *
      *  In other words, this case class and unapply are encoded the same:
      *
      *    case class Foo(x: Int, y: Int, zs: Char*)
      *    def unapplySeq(x: Foo): Option[(Int, Int, Seq[Char])]
      *
      *  Both are Extractor(Foo, Int :: Int :: Nil, Repeated(Seq[Char], Char, Char*))
      *
      *  @param  whole     The type in its unextracted form
      *  @param  fixed     The non-sequence types which are extracted
      *  @param  repeated  The sequence type which is extracted
      */
    /*final*/ case class Extractor(whole: Type, fixed: List[Type], repeated: Repeated) {
      require(whole != NoType, s"expandTypes($whole, $fixed, $repeated)")

      def prodArity    = fixed.length
      def hasSeq       = repeated.exists
      def elementType  = repeated.elementType
      def sequenceType = repeated.sequenceType
      def allTypes     = fixed ::: repeated.sequenceList
      def varargsTypes = fixed ::: repeated.repeatedList
      def isErroneous  = allTypes contains NoType

      private def typeStrings = fixed.map("" + _) ::: ( if (hasSeq) List("" + repeated) else Nil )

      def offeringString = if (isErroneous) "<error>" else typeStrings match {
        case Nil       => "Boolean"
        case tp :: Nil => tp
        case tps       => tps.mkString("(", ", ", ")")
      }
      override def toString = "%s => %s".format(whole, offeringString)
    }

    /*final*/ case class TypedPat(pat: Pattern, tpe: Type) {
      override def toString = s"$pat: $tpe"
    }

    /** If elementArity is...
      *    0: A perfect match between extractor and the fixed patterns.
      *       If there is a star pattern it will match any sequence.
      *  > 0: There are more patterns than products. There will have to be a
      *       sequence which can populate at least <elementArity> patterns.
      *  < 0: There are more products than patterns: compile time error.
      */
    /*final*/ case class Aligned(patterns: Patterns, extractor: Extractor) {
      def elementArity = patterns.nonStarArity - prodArity
      def prodArity    = extractor.prodArity
      def starArity    = patterns.starArity
      def totalArity   = patterns.totalArity

      def wholeType            = extractor.whole
      def sequenceType         = extractor.sequenceType
      def productTypes         = extractor.fixed
      def extractedTypes       = extractor.allTypes
      def typedNonStarPatterns = products ::: elements
      def typedPatterns        = typedNonStarPatterns ::: stars

      def isBool   = !isSeq && prodArity == 0
      def isSingle = !isSeq && totalArity == 1
      def isStar   = patterns.hasStar
      def isSeq    = extractor.hasSeq

      private def typedAsElement(pat: Pattern)  = TypedPat(pat, extractor.elementType)
      private def typedAsSequence(pat: Pattern) = TypedPat(pat, extractor.sequenceType)
      private def productPats = patterns.fixed take prodArity
      private def elementPats = patterns.fixed drop prodArity
      private def products    = (productPats, productTypes).zipped map TypedPat
      private def elements    = elementPats map typedAsElement
      private def stars       = patterns.starPatterns map typedAsSequence

      override def toString = s"""
      |Aligned {
      |   patterns  $patterns
      |  extractor  $extractor
      |    arities  $prodArity/$elementArity/$starArity  // product/element/star
      |      typed  ${typedPatterns mkString ", "}
      |}""".stripMargin.trim
    }
  }

  /** This is scalac-specific logic layered on top of the scalac-agnostic
    *  "matching products to patterns" logic defined in PatternExpander.
    */
  trait ScalacPatternExpanders {

    type PatternAligned = ScalacPatternExpander#Aligned

    implicit class AlignedOps(val aligned: PatternAligned) {
      import aligned._
      def expectedTypes     = typedPatterns map (_.tpe)
      def unexpandedFormals = extractor.varargsTypes
    }

    trait ScalacPatternExpander extends PatternExpander[Tree, Type] {
      def NoPattern = EmptyTree
      def NoType    = core.Types.NoType

      def newPatterns(patterns: List[Tree]): Patterns = patterns match {
        case init :+ last if tpd.isWildcardStarArg(last) => Patterns(init, last)
        case _                            => Patterns(patterns, NoPattern)
      }
      def typeOfMemberNamedHead(tpe: Type): Type = tpe.select(nme.head)
      def typeOfMemberNamedApply(tpe: Type): Type = tpe.select(nme.apply)

      def elementTypeOf(tpe: Type) = {
        val seq = tpe //repeatedToSeq(tpe)

        ( typeOfMemberNamedHead(seq)
          orElse typeOfMemberNamedApply(seq)
          orElse seq.elemType
        )
      }
      def newExtractor(whole: Type, fixed: List[Type], repeated: Repeated): Extractor = {
        ctx.log(s"newExtractor($whole, $fixed, $repeated")
        Extractor(whole, fixed, repeated)
      }

      // Turn Seq[A] into Repeated(Seq[A], A, A*)
      def repeatedFromSeq(seqType: Type): Repeated = {
        val elem     = elementTypeOf(seqType)
        val repeated = /*scalaRepeatedType(*/elem//)

        Repeated(seqType, elem, repeated)
      }
      // Turn A* into Repeated(Seq[A], A, A*)
      def repeatedFromVarargs(repeated: Type): Repeated =
        //Repeated(repeatedToSeq(repeated), repeatedToSingle(repeated), repeated)
        Repeated(repeated, repeated.elemType, repeated)

      /** In this case we are basing the pattern expansion on a case class constructor.
        *  The argument is the MethodType carried by the primary constructor.
        */
      def applyMethodTypes(method: Type): Extractor = {
        val whole = method.finalResultType

        method.paramInfoss.head match {
          case init :+ last if last.isRepeatedParam => newExtractor(whole, init, repeatedFromVarargs(last))
          case tps                                  => newExtractor(whole, tps, NoRepeated)
        }
      }

      def hasSelectors(tpe: Type) = tpe.member(nme._1).exists && tpe.member(nme._2).exists // dd todo: ???


      /** In this case, expansion is based on an unapply or unapplySeq method.
        *  Unfortunately the MethodType does not carry the information of whether
        *  it was unapplySeq, so we have to funnel that information in separately.
        */
      def unapplyMethodTypes(tree: Tree, fun: Tree, args: List[Tree], resultType: Type, isSeq: Boolean): Extractor = {
        _id = _id + 1

        val whole       = tree.tpe // see scaladoc for Trees.Unapply
              // fun.tpe.widen.paramTypess.headOption.flatMap(_.headOption).getOrElse(NoType)//firstParamType(method)
        val resultOfGet = extractorMemberType(resultType, nme.get)

        val expanded: List[Type] = /*(
          if (result =:= defn.BooleanType) Nil
          else if (defn.isProductSubType(result)) productSelectorTypes(result)
          else if (result.classSymbol is Flags.CaseClass) result.decls.filter(x => x.is(Flags.CaseAccessor) && x.is(Flags.Method)).map(_.info).toList
          else result.select(nme.get) :: Nil
          )*/
          if (isProductMatch(resultType, args.length)) productSelectorTypes(resultType)
          else if (isGetMatch(resultType)) getUnapplySelectors(resultOfGet, args)
          else if (resultType isRef defn.BooleanClass) Nil
          else {
            ctx.error(i"invalid return type in Unapply node: $resultType")
            Nil
          }

        expanded match {
          case init :+ last if isSeq => newExtractor(whole, init, repeatedFromSeq(last))
          case tps                   => newExtractor(whole, tps, NoRepeated)
        }
      }
    }

    object alignPatterns extends ScalacPatternExpander {
      private def validateAligned(tree: Tree, aligned: Aligned): Aligned = {
        import aligned._

        def owner         = tree.symbol.owner
        def offering      = extractor.offeringString
        def symString     = tree.symbol.showLocated
        def offerString   = if (extractor.isErroneous) "" else s" offering $offering"
        def arityExpected = (if (extractor.hasSeq) "at least " else "") + prodArity

        def err(msg: String)         = ctx.error(msg, tree.pos)
        def warn(msg: String)        = ctx.warning(msg, tree.pos)
        def arityError(what: String) = err(s"${_id} $what patterns for $owner$offerString: expected $arityExpected, found $totalArity")

        if (isStar && !isSeq)
          err("Star pattern must correspond with varargs or unapplySeq")
        else if (elementArity < 0)
          arityError("not enough")
        else if (elementArity > 0 && !extractor.hasSeq)
          arityError("too many")

        aligned
      }

      object Applied {
        // Duplicated with `spliceApply`
        def unapply(tree: Tree): Option[Tree] = tree match {
          // SI-7868 Admit Select() to account for numeric widening, e.g. <unappplySelector>.toInt
          /*case Apply(fun, (Ident(nme.SELECTOR_DUMMY)| Select(Ident(nme.SELECTOR_DUMMY), _)) :: Nil)
          => Some(fun)*/
          case Apply(fun, _) => unapply(fun)
          case _             => None
        }
      }

      def apply(tree: Tree, sel: Tree, args: List[Tree], resultType: Type): Aligned = {
        val fn = sel match {
          case Applied(fn) => fn
          case _           => sel
        }
        val patterns  = newPatterns(args)
        val isSeq = sel.symbol.name == nme.unapplySeq
        val extractor = sel.symbol.name match {
          case nme.unapply    => unapplyMethodTypes(tree, /*fn*/sel, args, resultType, isSeq = false)
          case nme.unapplySeq => unapplyMethodTypes(tree, /*fn*/sel, args, resultType, isSeq = true)
          case _              => applyMethodTypes(/*fn*/sel.tpe)
        }

        validateAligned(fn, Aligned(patterns, extractor))
      }

      def apply(tree: Tree, resultType: Type): Aligned = tree match {
        case Typed(tree, _)               => apply(tree, resultType)
        case Apply(fn, args)              => apply(tree, fn, args, resultType)
        case UnApply(fn, implicits, args) => apply(tree, fn, args, resultType)
      }
    }
  }
  }
}
