package dotty.tools.dotc
package transform

import ast.Trees._
import ast.{TreeTypeMap, tpd}
import core._
import Constants.Constant
import Contexts.Context
import Decorators._
import Symbols._
import StdNames.nme
import Types._
import NameKinds.{TailLabelName, TailLocalName, TailTempName}
import MegaPhase.MiniPhase
import reporting.diagnostic.messages.TailrecNotApplicable

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
 *  {{{
 *  var localForParam1: T1 = param1
 *  ...
 *  while (<empty>) {
 *    tailResult[ResultType]: {
 *      return {
 *        // original rhs with tail recursive calls transformed (see below)
 *      }
 *    }
 *  }
 *  }}}
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
 *  {{{
 *  def fact(n: Int, acc: Int): Int =
 *    if (n == 0) acc
 *    else fact(n - 1, acc * n)
 *  }}}
 *  is rewritten as:
 *  {{{
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
 *  }}}
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
  import TailRec._

  import dotty.tools.dotc.ast.tpd._

  override def phaseName: String = TailRec.name

  override def runsAfter: Set[String] = Set(Erasure.name) // tailrec assumes erased types

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context): tpd.Tree = {
    val sym = tree.symbol
    tree match {
      case dd@DefDef(name, Nil, vparams :: Nil, tpt, _)
        if (sym.isEffectivelyFinal) && !((sym is Flags.Accessor) || (dd.rhs eq EmptyTree)) =>
        val mandatory = sym.hasAnnotation(defn.TailrecAnnot)
        cpy.DefDef(dd)(rhs = {
          val defIsTopLevel = sym.owner.isClass
          val origMeth = sym
          val owner = ctx.owner.enclosingClass.asClass

          // Note: this can be split in two separate transforms(in different groups),
          // than first one will collect info about which transformations and rewritings should be applied
          // and second one will actually apply,
          // now this speculatively transforms tree and throws away result in many cases
          val transformer = new TailRecElimination(origMeth, owner, vparams.map(_.symbol), mandatory)
          val rhsSemiTransformed = transformer.transform(dd.rhs)

          if (transformer.rewrote) {
            val varForRewrittenThis = transformer.varForRewrittenThis
            val rewrittenParamSyms = transformer.rewrittenParamSyms
            val varsForRewrittenParamSyms = transformer.varsForRewrittenParamSyms

            val initialVarDefs = {
              val initialParamVarDefs = (rewrittenParamSyms, varsForRewrittenParamSyms).zipped.map {
                (param, local) => ValDef(local.asTerm, ref(param))
              }
              varForRewrittenThis match {
                case Some(local) => ValDef(local.asTerm, This(tree.symbol.owner.asClass)) :: initialParamVarDefs
                case none => initialParamVarDefs
              }
            }

            val rhsFullyTransformed = varForRewrittenThis match {
              case Some(localThisSym) =>
                val thisRef = localThisSym.termRef
                new TreeTypeMap(
                  typeMap = _.substThisUnlessStatic(owner, thisRef)
                    .subst(rewrittenParamSyms, varsForRewrittenParamSyms.map(_.termRef)),
                  treeMap = {
                    case tree: This if tree.symbol == owner => Ident(thisRef)
                    case tree => tree
                  }
                ).transform(rhsSemiTransformed)

              case none =>
                new TreeTypeMap(
                  typeMap = _.subst(rewrittenParamSyms, varsForRewrittenParamSyms.map(_.termRef))
                ).transform(rhsSemiTransformed)
            }

            Block(
              initialVarDefs,
              WhileDo(EmptyTree, {
                Labeled(transformer.continueLabel.asTerm, {
                  Return(rhsFullyTransformed, origMeth)
                })
              })
            )
          } else {
            if (mandatory) ctx.error(
              "TailRec optimisation not applicable, method not tail recursive",
              // FIXME: want to report this error on `dd.namePos`, but
              // because of extension method getting a weird pos, it is
              // better to report on symbol so there's no overlap
              sym.pos
            )
            dd.rhs
          }
        })
      case d: DefDef if d.symbol.hasAnnotation(defn.TailrecAnnot) =>
        ctx.error(TailrecNotApplicable(sym), sym.pos)
        d
      case _ => tree
    }

  }

  class TailRecElimination(method: Symbol, enclosingClass: Symbol, paramSyms: List[Symbol], isMandatory: Boolean) extends tpd.TreeMap {

    import dotty.tools.dotc.ast.tpd._

    var rewrote: Boolean = false

    /** The `tailLabelN` label symbol, used to encode a `continue` from the infinite `while` loop. */
    private[this] var myContinueLabel: Symbol = _
    def continueLabel(implicit c: Context): Symbol = {
      if (myContinueLabel == null)
        myContinueLabel = c.newSymbol(method, TailLabelName.fresh(), Flags.Label, defn.UnitType)
      myContinueLabel
    }

    /** The local `var` that replaces `this`, if it is modified in at least one recursive call. */
    var varForRewrittenThis: Option[Symbol] = None
    /** The subset of `paramSyms` that are modified in at least one recursive call, and which therefore need a replacement `var`. */
    var rewrittenParamSyms: List[Symbol] = Nil
    /** The replacement `var`s for the params in `rewrittenParamSyms`. */
    var varsForRewrittenParamSyms: List[Symbol] = Nil

    private def getVarForRewrittenThis()(implicit c: Context): Symbol = {
      varForRewrittenThis match {
        case Some(sym) => sym
        case none =>
          val tpe =
            if (enclosingClass.is(Flags.Module)) enclosingClass.thisType
            else enclosingClass.asClass.classInfo.selfType
          val sym = c.newSymbol(method, nme.SELF, Flags.Synthetic | Flags.Mutable, tpe)
          varForRewrittenThis = Some(sym)
          sym
      }
    }

    private def getVarForRewrittenParam(param: Symbol)(implicit c: Context): Symbol = {
      rewrittenParamSyms.indexOf(param) match {
        case -1 =>
          val sym = c.newSymbol(method, TailLocalName.fresh(param.name.toTermName), Flags.Synthetic | Flags.Mutable, param.info)
          rewrittenParamSyms ::= param
          varsForRewrittenParamSyms ::= sym
          sym
        case index => varsForRewrittenParamSyms(index)
      }
    }

    /** Symbols of Labeled blocks that are in tail position. */
    private val tailPositionLabeledSyms = new collection.mutable.HashSet[Symbol]()

    private[this] var ctx: TailContext = yesTailContext

    /** Rewrite this tree to contain no tail recursive calls */
    def transform(tree: Tree, nctx: TailContext)(implicit c: Context): Tree = {
      if (ctx == nctx) transform(tree)
      else {
        val saved = ctx
        ctx = nctx
        try transform(tree)
        finally this.ctx = saved
      }
    }

    def yesTailTransform(tree: Tree)(implicit c: Context): Tree =
      transform(tree, yesTailContext)

    def noTailTransform(tree: Tree)(implicit c: Context): Tree =
      transform(tree, noTailContext)

    def noTailTransforms[Tr <: Tree](trees: List[Tr])(implicit c: Context): List[Tr] =
      trees.mapConserve(noTailTransform).asInstanceOf[List[Tr]]

    override def transform(tree: Tree)(implicit c: Context): Tree = {
      /* Rewrite an Apply to be considered for tail call transformation. */
      def rewriteApply(tree: Apply): Tree = {
        val call = tree.fun
        val sym = call.symbol
        val arguments = noTailTransforms(tree.args)

        val prefix = call match {
          case Select(qual, _) => qual
          case x: Ident if x.symbol eq method => EmptyTree
          case x => x
        }

        val isRecursiveCall = (method eq sym)

        def continue =
          tpd.cpy.Apply(tree)(noTailTransform(call), arguments)

        def fail(reason: String) = {
          if (isMandatory) c.error(s"Cannot rewrite recursive call: $reason", tree.pos)
          else c.debuglog("Cannot rewrite recursive call at: " + tree.pos + " because: " + reason)
          continue
        }

        if (isRecursiveCall) {
          if (ctx.tailPos) {
            c.debuglog("Rewriting tail recursive call:  " + tree.pos)
            rewrote = true

            val assignParamPairs = for {
              (param, arg) <- paramSyms.zip(arguments)
              if (arg match {
                case arg: Ident => arg.symbol != param
                case _ => true
              })
            } yield {
              (getVarForRewrittenParam(param), arg)
            }

            val assignThisAndParamPairs = {
              if (prefix eq EmptyTree) assignParamPairs
              else {
                // TODO Opt: also avoid assigning `this` if the prefix is `this.`
                (getVarForRewrittenThis(), noTailTransform(prefix)) :: assignParamPairs
              }
            }

            val assignments = assignThisAndParamPairs match {
              case (lhs, rhs) :: Nil =>
                Assign(ref(lhs), rhs) :: Nil
              case _ :: _ =>
                val (tempValDefs, assigns) = (for ((lhs, rhs) <- assignThisAndParamPairs) yield {
                  val temp = c.newSymbol(method, TailTempName.fresh(lhs.name.toTermName), Flags.Synthetic, lhs.info)
                  (ValDef(temp, rhs), Assign(ref(lhs), ref(temp)).withPos(tree.pos))
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
            seq(assignments, Typed(Return(Literal(Constant(())).withPos(tree.pos), continueLabel), tpt))
          }
          else fail("it is not in tail position")
        } else {
          // FIXME `(method.name eq sym)` is always false (Name vs Symbol). What is this trying to do?
          val receiverIsSuper = (method.name eq sym) && enclosingClass.appliedRef.widen <:< prefix.tpe.widenDealias

          if (receiverIsSuper) fail("it contains a recursive call targeting a supertype")
          else continue
        }
      }

      def rewriteTry(tree: Try): Try = {
        if (tree.finalizer eq EmptyTree) {
          // SI-1672 Catches are in tail position when there is no finalizer
          tpd.cpy.Try(tree)(
            noTailTransform(tree.expr),
            transformSub(tree.cases),
            EmptyTree
          )
        }
        else {
          tpd.cpy.Try(tree)(
            noTailTransform(tree.expr),
            noTailTransforms(tree.cases),
            noTailTransform(tree.finalizer)
          )
        }
      }

      val res: Tree = tree match {
        case tree@Apply(fun, args) =>
          val meth = fun.symbol
          if (meth == defn.Boolean_|| || meth == defn.Boolean_&&)
            tpd.cpy.Apply(tree)(noTailTransform(fun), transform(args))
          else
            rewriteApply(tree)

        case tree: Select =>
          tpd.cpy.Select(tree)(noTailTransform(tree.qualifier), tree.name)

        case tree@Block(stats, expr) =>
          tpd.cpy.Block(tree)(
            noTailTransforms(stats),
            transform(expr)
          )

        case tree@If(cond, thenp, elsep) =>
          tpd.cpy.If(tree)(
            noTailTransform(cond),
            transform(thenp),
            transform(elsep)
          )

        case tree@CaseDef(_, _, body) =>
          cpy.CaseDef(tree)(body = transform(body))

        case tree@Match(selector, cases) =>
          tpd.cpy.Match(tree)(
            noTailTransform(selector),
            transformSub(cases)
          )

        case tree: Try =>
          rewriteTry(tree)

        case Alternative(_) | Bind(_, _) =>
          assert(false, "We should never have gotten inside a pattern")
          tree

        case ValDef(_, _, _) | DefDef(_, _, _, _, _) | EmptyTree | Super(_, _) | This(_) |
             Literal(_) | TypeTree() | TypeDef(_, _) =>
          tree

        case Labeled(bind, expr) =>
          if (ctx.tailPos)
            tailPositionLabeledSyms += bind.symbol
          tpd.cpy.Labeled(tree)(bind, transform(expr))

        case Return(expr, from) =>
          val fromSym = from.symbol
          val tailPos = fromSym.is(Flags.Label) && tailPositionLabeledSyms.contains(fromSym)
          tpd.cpy.Return(tree)(transform(expr, new TailContext(tailPos)), from)

        case _ =>
          super.transform(tree)
      }

      res
    }
  }
}

object TailRec {
  val name: String = "tailrec"

  final class TailContext(val tailPos: Boolean) extends AnyVal

  final val noTailContext: TailRec.TailContext = new TailContext(false)
  final val yesTailContext: TailRec.TailContext = new TailContext(true)
}
