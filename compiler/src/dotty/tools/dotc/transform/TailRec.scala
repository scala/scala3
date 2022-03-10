package dotty.tools.dotc
package transform

import ast.{TreeTypeMap, tpd}
import config.Printers.tailrec
import core.Contexts._
import core.Constants.Constant
import core.Flags._
import core.NameKinds.{TailLabelName, TailLocalName, TailTempName}
import core.StdNames.nme
import core.Symbols._
import reporting._
import transform.MegaPhase.MiniPhase
import util.LinearSet
import dotty.tools.uncheckedNN


/** A Tail Rec Transformer.
 *
 *  What it does:
 *
 *  Finds method calls in tail-position and replaces them with jumps.
 *  A call is in a tail-position if it is the last instruction to be
 *  executed in the body of a method. This includes being in
 *  tail-position of a `return` from a `Labeled` block which is itself
 *  in tail-position (which is critical for tail-recursive calls in the
 *  cases of a `match`). To identify tail positions, we recurse over
 *  the trees that may contain calls in tail-position (trees that can't
 *  contain such calls are not transformed).
 *
 *  When a method contains at least one tail-recursive call, its rhs
 *  is wrapped in the following structure:
 *
 *  ```
 *  var localForParam1: T1 = param1
 *  ...
 *  while (<empty>) {
 *    tailResult[ResultType]: {
 *      return {
 *        // original rhs with tail recursive calls transformed (see below)
 *      }
 *    }
 *  }
 *  ```
 *
 *  Self-recursive calls in tail-position are then replaced by (a)
 *  reassigning the local `var`s substituting formal parameters and
 *  (b) a `return` from the `tailResult` labeled block, which has the
 *  net effect of looping back to the beginning of the method.
 *  If the receiver is modifed in a recursive call, an additional `var`
 *  is used to replace `this`.
 *
 *  As a complete example of the transformation, the classical `fact`
 *  function, defined as:
 *
 *  ```
 *  def fact(n: Int, acc: Int): Int =
 *    if (n == 0) acc
 *    else fact(n - 1, acc * n)
 *  ```
 *
 *  is rewritten as:
 *
 *  ```
 *  def fact(n: Int, acc: Int): Int = {
 *    var acc$tailLocal1: Int = acc
 *    var n$tailLocal1: Int = n
 *    while (<empty>) {
 *      tailLabel1[Unit]: {
 *        return {
 *          if (n$tailLocal1 == 0)
 *            acc$tailLocal1
 *          else {
 *            val n$tailLocal1$tmp1: Int = n$tailLocal1 - 1
 *            val acc$tailLocal1$tmp1: Int = acc$tailLocal1 * n$tailLocal1
 *            n$tailLocal1 = n$tailLocal1$tmp1
 *            acc$tailLocal1 = acc$tailLocal1$tmp1
 *            (return[tailLabel1] ()): Int
 *          }
 *        }
 *      }
 *    }
 *  }
 *  ```
 *
 *  As the JVM provides no way to jump from a method to another one,
 *  non-recursive calls in tail-position are not optimized.
 *
 *  A method call is self-recursive if it calls the current method and
 *  the method is final (otherwise, it could be a call to an overridden
 *  method in a subclass). Recursive calls on a different instance are
 *  optimized.
 *
 *  This phase has been moved after erasure to allow the use of vars
 *  for the parameters combined with a `WhileDo`. This is also
 *  beneficial to support polymorphic tail-recursive calls.
 *
 *  In scalac, if the method had type parameters, the call must contain
 *  the same parameters as type arguments. This is no longer the case in
 *  dotc thanks to being located after erasure.
 *  In scalac, this is named tailCall but it does only provide optimization for
 *  self recursive functions, that's why it's renamed to tailrec
 *
 *  @author
 *    Erik Stenman, Iulian Dragos,
 *    ported and heavily modified for dotty by Dmitry Petrashko
 *    moved after erasure and adapted to emit `Labeled` blocks by Sébastien Doeraene
 */
class TailRec extends MiniPhase {
  import tpd._

  override def phaseName: String = TailRec.name

  override def description: String = TailRec.description

  override def runsAfter: Set[String] = Set(Erasure.name) // tailrec assumes erased types

  override def transformDefDef(tree: DefDef)(using Context): Tree = {
    val method = tree.symbol
    val mandatory = method.hasAnnotation(defn.TailrecAnnot)
    def noTailTransform(failureReported: Boolean) = {
      // FIXME: want to report this error on `tree.nameSpan`, but
      // because of extension method getting a weird position, it is
      // better to report on method symbol so there's no overlap.
      // We don't report a new error if failures were reported
      // during the transformation.
      if (mandatory && !failureReported)
        report.error(TailrecNotApplicable(method), method.srcPos)

      tree
    }

    val isCandidate = method.isEffectivelyFinal &&
      !(method.is(Accessor) || tree.rhs.eq(EmptyTree))

    if (isCandidate) {
      val enclosingClass = method.enclosingClass.asClass

      // Note: this can be split in two separate transforms(in different groups),
      // than first one will collect info about which transformations and rewritings should be applied
      // and second one will actually apply,
      // now this speculatively transforms tree and throws away result in many cases
      val transformer = new TailRecElimination(method, enclosingClass, tree.termParamss.head.map(_.symbol), mandatory)
      val rhsSemiTransformed = transformer.transform(tree.rhs)

      if (transformer.rewrote) {
        val varForRewrittenThis = transformer.varForRewrittenThis
        val rewrittenParamSyms = transformer.rewrittenParamSyms
        val varsForRewrittenParamSyms = transformer.varsForRewrittenParamSyms

        val initialVarDefs = {
          val initialParamVarDefs = rewrittenParamSyms.lazyZip(varsForRewrittenParamSyms).map {
            (param, local) => ValDef(local.asTerm, ref(param))
          }
          varForRewrittenThis match {
            case Some(local) => ValDef(local.asTerm, This(enclosingClass)) :: initialParamVarDefs
            case none => initialParamVarDefs
          }
        }

        val rhsFullyTransformed = varForRewrittenThis match {
          case Some(localThisSym) =>
            val thisRef = localThisSym.termRef
            new TreeTypeMap(
              typeMap = _.substThisUnlessStatic(enclosingClass, thisRef)
                .subst(rewrittenParamSyms, varsForRewrittenParamSyms.map(_.termRef)),
              treeMap = {
                case tree: This if tree.symbol == enclosingClass => Ident(thisRef)
                case tree => tree
              }
            ).transform(rhsSemiTransformed)

          case none =>
            new TreeTypeMap(
              typeMap = _.subst(rewrittenParamSyms, varsForRewrittenParamSyms.map(_.termRef))
            ).transform(rhsSemiTransformed)
        }

        /** Is the RHS a direct recursive tailcall, possibly with swapped arguments or modified pure arguments.
         *  ```
         *  def f(<params>): T = f(<args>)
         *  ```
         *  where `<args>` are pure arguments or references to parameters in `<params>`.
         */
        def isInfiniteRecCall(tree: Tree): Boolean = {
          def tailArgOrPureExpr(stat: Tree): Boolean = stat match {
            case stat: ValDef if stat.name.is(TailTempName) || !stat.symbol.is(Mutable) => tailArgOrPureExpr(stat.rhs)
            case Assign(lhs: Ident, rhs) if lhs.symbol.name.is(TailLocalName) => tailArgOrPureExpr(rhs)
            case Assign(lhs: Ident, rhs: Ident) => lhs.symbol == rhs.symbol
            case stat: Ident if stat.symbol.name.is(TailLocalName) => true
            case _ => tpd.isPureExpr(stat)
          }
          tree match {
            case Typed(expr, _) => isInfiniteRecCall(expr)
            case Return(Literal(Constant(())), label) => label.symbol == transformer.continueLabel
            case Block(stats, expr) => stats.forall(tailArgOrPureExpr) && isInfiniteRecCall(expr)
            case _ => false
          }
        }

        if isInfiniteRecCall(rhsFullyTransformed) then
          report.warning("Infinite recursive call", tree.srcPos)

        cpy.DefDef(tree)(rhs =
          Block(
            initialVarDefs,
            WhileDo(EmptyTree, {
              Labeled(transformer.continueLabel.asTerm, {
                Return(rhsFullyTransformed, method)
              })
            })
          )
        )
      }
      else noTailTransform(failureReported = transformer.failureReported)
    }
    else noTailTransform(failureReported = false)
  }

  class TailRecElimination(method: Symbol, enclosingClass: ClassSymbol, paramSyms: List[Symbol], isMandatory: Boolean) extends TreeMap {

    var rewrote: Boolean = false
    var failureReported: Boolean = false

    /** The `tailLabelN` label symbol, used to encode a `continue` from the infinite `while` loop. */
    private var myContinueLabel: Symbol | Null = _
    def continueLabel(using Context): Symbol = {
      if (myContinueLabel == null)
        myContinueLabel = newSymbol(method, TailLabelName.fresh(), Label, defn.UnitType)
      myContinueLabel.uncheckedNN
    }

    /** The local `var` that replaces `this`, if it is modified in at least one recursive call. */
    var varForRewrittenThis: Option[Symbol] = None
    /** The subset of `paramSyms` that are modified in at least one recursive call, and which therefore need a replacement `var`. */
    var rewrittenParamSyms: List[Symbol] = Nil
    /** The replacement `var`s for the params in `rewrittenParamSyms`. */
    var varsForRewrittenParamSyms: List[Symbol] = Nil

    private def getVarForRewrittenThis()(using Context): Symbol =
      varForRewrittenThis match {
        case Some(sym) => sym
        case none =>
          val tpe =
            if (enclosingClass.is(Module)) enclosingClass.thisType
            else enclosingClass.classInfo.selfType
          val sym = newSymbol(method, nme.SELF, Synthetic | Mutable, tpe)
          varForRewrittenThis = Some(sym)
          sym
      }

    private def getVarForRewrittenParam(param: Symbol)(using Context): Symbol =
      rewrittenParamSyms.indexOf(param) match {
        case -1 =>
          val sym = newSymbol(method, TailLocalName.fresh(param.name.toTermName), Synthetic | Mutable, param.info)
          rewrittenParamSyms ::= param
          varsForRewrittenParamSyms ::= sym
          sym
        case index => varsForRewrittenParamSyms(index)
      }

    /** Symbols of Labeled blocks that are in tail position. */
    private var tailPositionLabeledSyms = LinearSet.empty[Symbol]

    private var inTailPosition = true

    /** Rewrite this tree to contain no tail recursive calls */
    def transform(tree: Tree, tailPosition: Boolean)(using Context): Tree =
      if (inTailPosition == tailPosition) transform(tree)
      else {
        val saved = inTailPosition
        inTailPosition = tailPosition
        try transform(tree)
        finally inTailPosition = saved
      }

    def yesTailTransform(tree: Tree)(using Context): Tree =
      transform(tree, tailPosition = true)

    def noTailTransform(tree: Tree)(using Context): Tree =
      transform(tree, tailPosition = false)

    def noTailTransforms[Tr <: Tree](trees: List[Tr])(using Context): List[Tr] =
      trees.mapConserve(noTailTransform).asInstanceOf[List[Tr]]

    override def transform(tree: Tree)(using Context): Tree = {
      /* Rewrite an Apply to be considered for tail call transformation. */
      def rewriteApply(tree: Apply): Tree = {
        val arguments = noTailTransforms(tree.args)

        def continue =
          cpy.Apply(tree)(noTailTransform(tree.fun), arguments)

        def fail(reason: String) = {
          if (isMandatory) {
            failureReported = true
            report.error(s"Cannot rewrite recursive call: $reason", tree.srcPos)
          }
          else
            tailrec.println("Cannot rewrite recursive call at: " + tree.span + " because: " + reason)
          continue
        }

        val calledMethod = tree.fun.symbol
        val prefix = tree.fun match {
          case Select(qual, _) => qual
          case x: Ident if x.symbol eq method => EmptyTree
          case x => x
        }

        val isRecursiveCall = calledMethod eq method
        def isRecursiveSuperCall = (method.name eq calledMethod.name) &&
          method.matches(calledMethod) &&
          enclosingClass.appliedRef.widen <:< prefix.tpe.widenDealias

        if (isRecursiveCall)
          if (inTailPosition) {
            tailrec.println("Rewriting tail recursive call:  " + tree.span)
            rewrote = true

            val assignParamPairs = for {
              (param, arg) <- paramSyms.zip(arguments)
              if (arg match {
                case arg: Ident => arg.symbol != param
                case _ => true
              })
            }
            yield
              (getVarForRewrittenParam(param), arg)

            val assignThisAndParamPairs =
              if (prefix eq EmptyTree) assignParamPairs
              else
                // TODO Opt: also avoid assigning `this` if the prefix is `this.`
                (getVarForRewrittenThis(), noTailTransform(prefix)) :: assignParamPairs

            val assignments = assignThisAndParamPairs match {
              case (lhs, rhs) :: Nil =>
                Assign(ref(lhs), rhs) :: Nil
              case _ :: _ =>
                val (tempValDefs, assigns) = (for ((lhs, rhs) <- assignThisAndParamPairs) yield {
                  val temp = newSymbol(method, TailTempName.fresh(lhs.name.toTermName), Synthetic, lhs.info)
                  (ValDef(temp, rhs), Assign(ref(lhs), ref(temp)).withSpan(tree.span))
                }).unzip
                tempValDefs ::: assigns
              case nil =>
                Nil
            }

            /* The `Typed` node is necessary to perfectly preserve the type of the node.
             * Without it, lubbing in enclosing if/else or match can infer a different type,
             * which can cause Ycheck errors.
             */
            val tpt = TypeTree(method.info.resultType)
            seq(assignments, Typed(Return(unitLiteral.withSpan(tree.span), continueLabel), tpt))
          }
          else fail("it is not in tail position")
        else if (isRecursiveSuperCall)
          fail("it targets a supertype")
        else
          continue
      }

      tree match {
        case tree @ Apply(fun, args) =>
          val meth = fun.symbol
          if (meth == defn.Boolean_|| || meth == defn.Boolean_&&)
            cpy.Apply(tree)(noTailTransform(fun), transform(args))
          else
            rewriteApply(tree)

        case tree @ Select(qual, name) =>
          cpy.Select(tree)(noTailTransform(qual), name)

        case tree @ Block(stats, expr) =>
          cpy.Block(tree)(
            noTailTransforms(stats),
            transform(expr)
          )

        case tree @ If(cond, thenp, elsep) =>
          cpy.If(tree)(
            noTailTransform(cond),
            transform(thenp),
            transform(elsep)
          )

        case tree: CaseDef =>
          cpy.CaseDef(tree)(body = transform(tree.body))

        case tree @ Match(selector, cases) =>
          cpy.Match(tree)(
            noTailTransform(selector),
            transformSub(cases)
          )

        case tree: Try =>
          val expr = noTailTransform(tree.expr)
          if (tree.finalizer eq EmptyTree)
            // SI-1672 Catches are in tail position when there is no finalizer
            cpy.Try(tree)(expr, transformSub(tree.cases), EmptyTree)
          else cpy.Try(tree)(
            expr,
            noTailTransforms(tree.cases),
            noTailTransform(tree.finalizer)
          )

        case tree @ WhileDo(cond, body) =>
          cpy.WhileDo(tree)(
            noTailTransform(cond),
            noTailTransform(body)
          )

        case _: Alternative | _: Bind =>
          assert(false, "We should never have gotten inside a pattern")
          tree

        case tree: ValOrDefDef =>
          if (isMandatory) noTailTransform(tree.rhs)
          tree

        case _: Super | _: This | _: Literal | _: TypeTree | _: TypeDef | EmptyTree =>
          tree

        case Labeled(bind, expr) =>
          if (inTailPosition)
            tailPositionLabeledSyms += bind.symbol
          try cpy.Labeled(tree)(bind, transform(expr))
          finally
            if (inTailPosition)
              tailPositionLabeledSyms -= bind.symbol

        case Return(expr, from) =>
          val fromSym = from.symbol
          val inTailPosition = !fromSym.is(Label) || tailPositionLabeledSyms.contains(fromSym)
          cpy.Return(tree)(transform(expr, inTailPosition), from)

        case _ =>
          super.transform(tree)
      }
    }
  }
}

object TailRec {
  val name: String = "tailrec"
  val description: String = "rewrite tail recursion to loops"
}
