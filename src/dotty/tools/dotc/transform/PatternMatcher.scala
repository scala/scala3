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
import dotty.tools.dotc.transform.PatternMatcher.CodegenCore.Casegen
import dotty.tools.dotc.util.Positions
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
  final case class Suppression(exhaustive: Boolean, unreachable: Boolean)
  object Suppression {
    val NoSuppression = Suppression(false, false)
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // the making of the trees
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait TreeMakers /*extends TypedSubstitution*/ extends CodegenCore {
    def optimizeCases(prevBinder: Symbol, cases: List[List[TreeMaker]], pt: Type): (List[List[TreeMaker]], List[Tree])
    def analyzeCases(prevBinder: Symbol, cases: List[List[TreeMaker]], pt: Type, suppression: Suppression): Unit

    def emitSwitch(scrut: Tree, scrutSym: Symbol, cases: List[List[TreeMaker]], pt: Type, matchFailGenOverride: Option[Tree => Tree], unchecked: Boolean): Option[Tree] =
      None

    // for catch (no need to customize match failure)
    def emitTypeSwitch(bindersAndCases: List[(Symbol, List[TreeMaker])], pt: Type): Option[List[CaseDef]] =
      None

    abstract class TreeMaker{
      def pos: Position

      /** captures the scope and the value of the bindings in patterns
        * important *when* the substitution happens (can't accumulate and do at once after the full matcher has been constructed)
        */
      /*def substitution: Substitution =
        if (currSub eq null) localSubstitution
        else currSub*/

      //protected def localSubstitution: Substitution

      /*private[TreeMakers] def incorporateOuterSubstitution(outerSubst: Substitution): Unit = {
        if (currSub ne null) {
          debug.patmat("BUG: incorporateOuterSubstitution called more than once for "+ ((this, currSub, outerSubst)))
          Thread.dumpStack()
        }
        else currSub = outerSubst >> substitution
      }
      private[this] var currSub: Substitution = null*/

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
      // def subPatternsAsSubstitution: Substitution = substitution

      // build Tree that chains `next` after the current extractor
      def chainBefore(next: Tree)(casegen: Casegen): Tree
    }

    sealed trait NoNewBinders extends TreeMaker {
      //protected val localSubstitution: Substitution = EmptySubstitution
    }

    case class TrivialTreeMaker(tree: Tree) extends TreeMaker with NoNewBinders {
      def pos = tree.pos

      def chainBefore(next: Tree)(casegen: Casegen): Tree = tree
    }

    case class BodyTreeMaker(body: Tree, matchPt: Type) extends TreeMaker with NoNewBinders {
      def pos = body.pos

      def chainBefore(next: Tree)(casegen: Casegen): Tree = // assert(next eq EmptyTree)
        /*atPos(body.pos)*/(casegen.one(/*substitution(*/body/*)*/)) // since SubstOnly treemakers are dropped, need to do it here
      override def toString = "B"+((body, matchPt))
    }

    case class SubstOnlyTreeMaker(prevBinder: Symbol, nextBinder: Symbol) extends TreeMaker {
      val pos = Positions.NoPosition

      //val localSubstitution = Substitution(prevBinder, CODE.REF(nextBinder))
      def chainBefore(next: Tree)(casegen: Casegen): Tree = /*substitution(*/next//)
      override def toString = "S"//+ localSubstitution
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

      lazy val nextBinder = freshSym(pos, nextBinderTp)
      //lazy val localSubstitution = Substitution(List(prevBinder), List(CODE.REF(nextBinder)))

      def chainBefore(next: Tree)(casegen: Casegen): Tree =
        /*atPos(pos)(*/casegen.flatMapCond(cond, res, nextBinder, /*substitution(*/next/*)*/))
    }

    // unless we're optimizing, emit local variable bindings for all subpatterns of extractor/case class patterns
    protected val debugInfoEmitVars = true //!settings.optimise.value

    sealed trait PreserveSubPatBinders extends TreeMaker {
      val subPatBinders: List[Symbol]
      val subPatRefs: List[Tree]
      val ignoredSubPatBinders: Set[Symbol]

      // unless `debugInfoEmitVars`, this set should contain the bare minimum for correctness
      // mutable case class fields need to be stored regardless (SI-5158, SI-6070) -- see override in ProductExtractorTreeMaker
      // sub patterns bound to wildcard (_) are never stored as they can't be referenced
      // dirty debuggers will have to get dirty to see the wildcards
      lazy val storedBinders: Set[Symbol] =
        (if (debugInfoEmitVars) subPatBinders.toSet else Set.empty) ++ extraStoredBinders -- ignoredSubPatBinders

      // e.g., mutable fields of a case class in ProductExtractorTreeMaker
      def extraStoredBinders: Set[Symbol]

      def emitVars = storedBinders.nonEmpty

      private lazy val (stored, substed) = (subPatBinders, subPatRefs).zipped.partition{ case (sym, _) => storedBinders(sym) }

      protected lazy val localSubstitution: Substitution = if (!emitVars) Substitution(subPatBinders, subPatRefs)
      else {
        val (subPatBindersSubstituted, subPatRefsSubstituted) = substed.unzip
        Substitution(subPatBindersSubstituted.toList, subPatRefsSubstituted.toList)
      }

      /** The substitution that specifies the trees that compute the values of the subpattern binders.
        *
        * We pretend to replace the subpattern binders by subpattern refs
        * (Even though we don't do so anymore -- see SI-5158, SI-5739 and SI-6070.)
        */
      override def subPatternsAsSubstitution =
        Substitution(subPatBinders, subPatRefs) >> super.subPatternsAsSubstitution

      def bindSubPats(in: Tree): Tree =
        if (!emitVars) in
        else {
          // binders in `subPatBindersStored` that are referenced by tree `in`
          val usedBinders = new mutable.HashSet[Symbol]()
          // all potentially stored subpat binders
          val potentiallyStoredBinders = stored.unzip._1.toSet
          // compute intersection of all symbols in the tree `in` and all potentially stored subpat binders
          in.foreach(t => if (potentiallyStoredBinders(t.symbol)) usedBinders += t.symbol)

          if (usedBinders.isEmpty) in
          else {
            // only store binders actually used
            val (subPatBindersStored, subPatRefsStored) = stored.filter{case (b, _) => usedBinders(b)}.unzip
            Block(map2(subPatBindersStored.toList, subPatRefsStored.toList)(ValDef(_, _)), in)
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

      debug.patmat(s"""
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
          case Some(cond) =>
            casegen.ifThenElseZero(substitution(cond), bindSubPats(substitution(next)))
          case _ =>
            bindSubPats(substitution(next))
        }
        atPos(extractor.pos)(
          if (extractorReturnsBoolean) casegen.flatMapCond(extractor, CODE.UNIT, nextBinder, condAndNext)
          else casegen.flatMap(extractor, nextBinder, condAndNext)
        )
      }

      override def toString = "X"+((extractor, nextBinder.name))
    }

    /**
     * An optimized version of ExtractorTreeMaker for Products.
     * For now, this is hard-coded to case classes, and we simply extract the case class fields.
     *
     * The values for the subpatterns, as specified by the case class fields at the time of extraction,
     * are stored in local variables that re-use the symbols in `subPatBinders`.
     * This makes extractor patterns more debuggable (SI-5739) as well as
     * avoiding mutation after the pattern has been matched (SI-5158, SI-6070)
     *
     * TODO: make this user-definable as follows
     *   When a companion object defines a method `def unapply_1(x: T): U_1`, but no `def unapply` or `def unapplySeq`,
     *   the extractor is considered to match any non-null value of type T
     *   the pattern is expected to have as many sub-patterns as there are `def unapply_I(x: T): U_I` methods,
     *   and the type of the I'th sub-pattern is `U_I`.
     *   The same exception for Seq patterns applies: if the last extractor is of type `Seq[U_N]`,
     *   the pattern must have at least N arguments (exactly N if the last argument is annotated with `: _*`).
     *   The arguments starting at N (and beyond) are taken from the sequence returned by apply_N,
     *   and it is checked that that sequence has enough elements to provide values for all expected sub-patterns.
     *
     *   For a case class C, the implementation is assumed to be `def unapply_I(x: C) = x._I`,
     *   and the extractor call is inlined under that assumption.
     */
    case class ProductExtractorTreeMaker(prevBinder: Symbol, extraCond: Option[Tree])(
      val subPatBinders: List[Symbol],
      val subPatRefs: List[Tree],
      val mutableBinders: List[Symbol],
      binderKnownNonNull: Boolean,
      val ignoredSubPatBinders: Set[Symbol]
      ) extends FunTreeMaker with PreserveSubPatBinders {

      import CODE._
      val nextBinder = prevBinder // just passing through

      // mutable binders must be stored to avoid unsoundness or seeing mutation of fields after matching (SI-5158, SI-6070)
      def extraStoredBinders: Set[Symbol] = mutableBinders.toSet

      def chainBefore(next: Tree)(casegen: Casegen): Tree = {
        val nullCheck = REF(prevBinder) OBJ_NE NULL
        val cond =
          if (binderKnownNonNull) extraCond
          else (extraCond map (nullCheck AND _)
            orElse Some(nullCheck))

        cond match {
          case Some(cond) =>
            casegen.ifThenElseZero(cond, bindSubPats(substitution(next)))
          case _ =>
            bindSubPats(substitution(next))
        }
      }

      override def toString = "P"+((prevBinder.name,  extraCond getOrElse "", localSubstitution))
    }

    object IrrefutableExtractorTreeMaker {
      // will an extractor with unapply method of methodtype `tp` always succeed?
      // note: this assumes the other side-conditions implied by the extractor are met
      // (argument of the right type, length check succeeds for unapplySeq,...)
      def irrefutableExtractorType(tp: Type): Boolean = tp.resultType.dealias match {
        case TypeRef(_, SomeClass, _) => true
        // probably not useful since this type won't be inferred nor can it be written down (yet)
        case ConstantTrue => true
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

      object treeCondStrategy extends TypeTestCondStrategy { import CODE._
        type Result = Tree

        def and(a: Result, b: Result): Result                = a AND b
        def tru                                              = mkTRUE
        def typeTest(testedBinder: Symbol, expectedTp: Type) = codegen._isInstanceOf(testedBinder, expectedTp)
        def nonNullTest(testedBinder: Symbol)                = REF(testedBinder) OBJ_NE NULL
        def equalsTest(pat: Tree, testedBinder: Symbol)      = codegen._equals(pat, testedBinder)
        def eqTest(pat: Tree, testedBinder: Symbol)          = REF(testedBinder) OBJ_EQ pat

        def outerTest(testedBinder: Symbol, expectedTp: Type): Tree = {
          val expectedOuter = expectedTp.prefix match {
            case ThisType(clazz) => This(clazz)
            case NoType          => mkTRUE // fallback for SI-6183
            case pre             => REF(pre.prefix, pre.termSymbol)
          }

          // ExplicitOuter replaces `Select(q, outerSym) OBJ_EQ expectedPrefix` by `Select(q, outerAccessor(outerSym.owner)) OBJ_EQ expectedPrefix`
          // if there's an outer accessor, otherwise the condition becomes `true` -- TODO: can we improve needsOuterTest so there's always an outerAccessor?
          val outer = expectedTp.typeSymbol.newMethod(vpmName.outer, newFlags = SYNTHETIC | ARTIFACT) setInfo expectedTp.prefix

          (Select(codegen._asInstanceOf(testedBinder, expectedTp), outer)) OBJ_EQ expectedOuter
        }
      }

      object pureTypeTestChecker extends TypeTestCondStrategy {
        type Result = Boolean

        def typeTest(testedBinder: Symbol, expectedTp: Type): Result  = true

        def outerTest(testedBinder: Symbol, expectedTp: Type): Result = false
        def nonNullTest(testedBinder: Symbol): Result                 = false
        def equalsTest(pat: Tree, testedBinder: Symbol): Result       = false
        def eqTest(pat: Tree, testedBinder: Symbol): Result           = false
        def and(a: Result, b: Result): Result                         = false // we don't and type tests, so the conjunction must include at least one false
        def tru                                                       = true
      }

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
    case class TypeTestTreeMaker(prevBinder: Symbol, testedBinder: Symbol, expectedTp: Type, nextBinderTp: Type)(override val pos: Position, extractorArgTypeTest: Boolean = false) extends CondTreeMaker {
      import TypeTestTreeMaker._
      debug.patmat("TTTM"+((prevBinder, extractorArgTypeTest, testedBinder, expectedTp, nextBinderTp)))

      lazy val outerTestNeeded = (
        (expectedTp.prefix ne NoPrefix)
          && !expectedTp.prefix.typeSymbol.isPackageClass
          && needsOuterTest(expectedTp, testedBinder.info, matchOwner)
        )

      // the logic to generate the run-time test that follows from the fact that
      // a `prevBinder` is expected to have type `expectedTp`
      // the actual tree-generation logic is factored out, since the analyses generate Cond(ition)s rather than Trees
      // TODO: `null match { x : T }` will yield a check that (indirectly) tests whether `null ne null`
      // don't bother (so that we don't end up with the warning "comparing values of types Null and Null using `ne' will always yield false")
      def renderCondition(cs: TypeTestCondStrategy): cs.Result = {
        import cs._

        // propagate expected type
        def expTp(t: Tree): t.type = t setType expectedTp

        def testedWide              = testedBinder.info.widen
        def expectedWide            = expectedTp.widen
        def isAnyRef                = testedWide <:< AnyRefTpe
        def isAsExpected            = testedWide <:< expectedTp
        def isExpectedPrimitiveType = isAsExpected && isPrimitiveValueType(expectedTp)
        def isExpectedReferenceType = isAsExpected && (expectedTp <:< AnyRefTpe)
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
          case SingleType(_, sym)                       => mkEqTest(gen.mkAttributedQualifier(expectedTp)) // SI-4577, SI-4897
          case ThisType(sym) if sym.isModule            => and(mkEqualsTest(CODE.REF(sym)), mkTypeTest) // must use == to support e.g. List() == Nil
          case ConstantType(Constant(null)) if isAnyRef => mkEqTest(expTp(CODE.NULL))
          case ConstantType(const)                      => mkEqualsTest(expTp(Literal(const)))
          case ThisType(sym)                            => mkEqTest(expTp(This(sym)))
          case _                                        => mkDefault
        }
      }

      val cond = renderCondition(treeCondStrategy)
      val res  = codegen._asInstanceOf(testedBinder, nextBinderTp)

      // is this purely a type test, e.g. no outer check, no equality tests (used in switch emission)
      def isPureTypeTest = renderCondition(pureTypeTestChecker)

      def impliesBinderNonNull(binder: Symbol) = renderCondition(nonNullImpliedByTestChecker(binder))

      override def toString = "TT"+((expectedTp, testedBinder.name, nextBinderTp))
    }

    // need to substitute to deal with existential types -- TODO: deal with existentials better, don't substitute (see RichClass during quick.comp)
    case class EqualityTestTreeMaker(prevBinder: Symbol, patTree: Tree, override val pos: Position) extends CondTreeMaker {
      val nextBinderTp = prevBinder.info.widen

      // NOTE: generate `patTree == patBinder`, since the extractor must be in control of the equals method (also, patBinder may be null)
      // equals need not be well-behaved, so don't intersect with pattern's (stabilized) type (unlike MaybeBoundTyped's accumType, where it's required)
      val cond = codegen._equals(patTree, prevBinder)
      val res  = CODE.REF(prevBinder)
      override def toString = "ET"+((prevBinder.name, patTree))
    }

    case class AlternativesTreeMaker(prevBinder: Symbol, var altss: List[List[TreeMaker]], pos: Position) extends TreeMaker with NoNewBinders {
      // don't substitute prevBinder to nextBinder, a set of alternatives does not need to introduce a new binder, simply reuse the previous one

      override private[TreeMakers] def incorporateOuterSubstitution(outerSubst: Substitution): Unit = {
        super.incorporateOuterSubstitution(outerSubst)
        altss = altss map (alts => propagateSubstitution(alts, substitution))
      }

      def chainBefore(next: Tree)(codegenAlt: Casegen): Tree = {
        atPos(pos){
          // one alternative may still generate multiple trees (e.g., an extractor call + equality test)
          // (for now,) alternatives may not bind variables (except wildcards), so we don't care about the final substitution built internally by makeTreeMakers
          val combinedAlts = altss map (altTreeMakers =>
            ((casegen: Casegen) => combineExtractors(altTreeMakers :+ TrivialTreeMaker(casegen.one(mkTRUE)))(casegen))
            )

          val findAltMatcher = codegenAlt.matcher(EmptyTree, NoSymbol, BooleanTpe)(combinedAlts, Some(x => mkFALSE))
          codegenAlt.ifThenElseZero(findAltMatcher, substitution(next))
        }
      }
    }

    case class GuardTreeMaker(guardTree: Tree) extends TreeMaker with NoNewBinders {
      val pos = guardTree.pos

      def chainBefore(next: Tree)(casegen: Casegen): Tree = casegen.flatMapGuard(substitution(guardTree), next)
      override def toString = "G("+ guardTree +")"
    }

    // combineExtractors changes the current substitution's of the tree makers in `treeMakers`
    // requires propagateSubstitution(treeMakers) has been called
    def combineExtractors(treeMakers: List[TreeMaker])(casegen: Casegen): Tree =
      treeMakers.foldRight(EmptyTree: Tree)((a, b) => a.chainBefore(b)(casegen))


    def removeSubstOnly(makers: List[TreeMaker]) = makers filterNot (_.isInstanceOf[SubstOnlyTreeMaker])

    // a foldLeft to accumulate the localSubstitution left-to-right
    // it drops SubstOnly tree makers, since their only goal in life is to propagate substitutions to the next tree maker, which is fullfilled by propagateSubstitution
    def propagateSubstitution(treeMakers: List[TreeMaker], initial: Substitution): List[TreeMaker] = {
      var accumSubst: Substitution = initial
      treeMakers foreach { maker =>
        maker incorporateOuterSubstitution accumSubst
        accumSubst = maker.substitution
      }
      removeSubstOnly(treeMakers)
    }

    // calls propagateSubstitution on the treemakers
    def combineCases(scrut: Tree, scrutSym: Symbol, casesRaw: List[List[TreeMaker]], pt: Type, owner: Symbol, matchFailGenOverride: Option[Tree => Tree]): Tree = {
      // drops SubstOnlyTreeMakers, since their effect is now contained in the TreeMakers that follow them
      val casesNoSubstOnly = casesRaw map (propagateSubstitution(_, EmptySubstitution))
      combineCasesNoSubstOnly(scrut, scrutSym, casesNoSubstOnly, pt, owner, matchFailGenOverride)
    }

    // pt is the fully defined type of the cases (either pt or the lub of the types of the cases)
    def combineCasesNoSubstOnly(scrut: Tree, scrutSym: Symbol, casesNoSubstOnly: List[List[TreeMaker]], pt: Type, owner: Symbol, matchFailGenOverride: Option[Tree => Tree]): Tree =
      fixerUpper(owner, scrut.pos) {
        def matchFailGen = matchFailGenOverride orElse Some(Throw(MatchErrorClass.tpe, _: Tree))

        debug.patmat("combining cases: "+ (casesNoSubstOnly.map(_.mkString(" >> ")).mkString("{", "\n", "}")))

        val (suppression, requireSwitch): (Suppression, Boolean) =
          if (settings.XnoPatmatAnalysis) (Suppression.NoSuppression, false)
          else scrut match {
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
          }

        emitSwitch(scrut, scrutSym, casesNoSubstOnly, pt, matchFailGenOverride, suppression.exhaustive).getOrElse{
          if (requireSwitch) typer.context.unit.warning(scrut.pos, "could not emit switch for @switch annotated match")

          if (casesNoSubstOnly nonEmpty) {
            // before optimizing, check casesNoSubstOnly for presence of a default case,
            // since DCE will eliminate trivial cases like `case _ =>`, even if they're the last one
            // exhaustivity and reachability must be checked before optimization as well
            // TODO: improve notion of trivial/irrefutable -- a trivial type test before the body still makes for a default case
            //   ("trivial" depends on whether we're emitting a straight match or an exception, or more generally, any supertype of scrutSym.tpe is a no-op)
            //   irrefutability checking should use the approximation framework also used for CSE, unreachability and exhaustivity checking
            val synthCatchAll =
              if (casesNoSubstOnly.nonEmpty && {
                val nonTrivLast = casesNoSubstOnly.last
                nonTrivLast.nonEmpty && nonTrivLast.head.isInstanceOf[BodyTreeMaker]
              }) None
              else matchFailGen

            analyzeCases(scrutSym, casesNoSubstOnly, pt, suppression)

            val (cases, toHoist) = optimizeCases(scrutSym, casesNoSubstOnly, pt)

            val matchRes = codegen.matcher(scrut, scrutSym, pt)(cases map combineExtractors, synthCatchAll)

            if (toHoist isEmpty) matchRes else Block(toHoist, matchRes)
          } else {
            codegen.matcher(scrut, scrutSym, pt)(Nil, matchFailGen)
          }
        }
      }

    // TODO: do this during tree construction, but that will require tracking the current owner in treemakers
    // TODO: assign more fine-grained positions
    // fixes symbol nesting, assigns positions
    protected def fixerUpper(origOwner: Symbol, pos: Position) = new Traverser {
      currentOwner = origOwner

      override def traverse(t: Tree) {
        if (t != EmptyTree && t.pos == NoPosition) {
          t.setPos(pos)
        }
        t match {
          case Function(_, _) if t.symbol == NoSymbol =>
            t.symbol = currentOwner.newAnonymousFunctionValue(t.pos)
            debug.patmat("new symbol for "+ ((t, t.symbol.ownerChain)))
          case Function(_, _) if (t.symbol.owner == NoSymbol) || (t.symbol.owner == origOwner) =>
            debug.patmat("fundef: "+ ((t, t.symbol.ownerChain, currentOwner.ownerChain)))
            t.symbol.owner = currentOwner
          case d : DefTree if (d.symbol != NoSymbol) && ((d.symbol.owner == NoSymbol) || (d.symbol.owner == origOwner)) => // don't indiscriminately change existing owners! (see e.g., pos/t3440, pos/t3534, pos/unapplyContexts2)
            debug.patmat("def: "+ ((d, d.symbol.ownerChain, currentOwner.ownerChain)))

            d.symbol.moduleClass andAlso (_.owner = currentOwner)
            d.symbol.owner = currentOwner
            // TODO DD:
            // case _ if (t.symbol != NoSymbol) && (t.symbol ne null) =>
            debug.patmat("untouched "+ ((t, t.getClass, t.symbol.ownerChain, currentOwner.ownerChain)))
          case _ =>
        }
        super.traverse(t)
      }

      // override def apply
      // debug.patmat("before fixerupper: "+ xTree)
      // currentRun.trackerFactory.snapshot()
      // debug.patmat("after fixerupper")
      // currentRun.trackerFactory.snapshot()
    }
  }

  trait MatchOptimizer extends OptimizedCodegen
  /*with SwitchEmission // todo: toBe ported
  with CommonSubconditionElimination*/ {
    override def optimizeCases(prevBinder: Symbol, cases: List[List[TreeMaker]], pt: Type): (List[List[TreeMaker]], List[Tree]) = {
      // TODO: do CSE on result of doDCE(prevBinder, cases, pt)
      val optCases = cases// todo: doCSE(prevBinder, cases, pt)
      val toHoist = (
        for (treeMakers <- optCases)
        yield treeMakers.collect{case tm: ReusedCondTreeMaker => tm.treesToHoist}
        ).flatten.flatten.toList
      (optCases, toHoist)
    }
  }
}