package dotty.tools.dotc
package transform

import ast.Trees._
import ast.{TreeTypeMap, tpd}
import core._
import Contexts.Context
import Decorators._
import Symbols._
import StdNames.nme
import Types._
import NameKinds.TailLabelName
import MegaPhase.MiniPhase
import reporting.diagnostic.messages.TailrecNotApplicable
import util.Property

/**
 * A Tail Rec Transformer
 * @author     Erik Stenman, Iulian Dragos,
 *             ported and heavily modified for dotty by Dmitry Petrashko
 *             moved after erasure by Sébastien Doeraene
 * @version    1.1
 *
 *             What it does:
 *             <p>
 *             Finds method calls in tail-position and replaces them with jumps.
 *             A call is in a tail-position if it is the last instruction to be
 *             executed in the body of a method. This includes being in
 *             tail-position of a `return` from a `Labeled` block which is itself
 *             in tail-position (which is critical for tail-recursive calls in the
 *             cases of a `match`). To identify tail positions, we recurse over
 *             the trees that may contain calls in tail-position (trees that can't
 *             contain such calls are not transformed).
 *             </p>
 *             <p>
 *             Self-recursive calls in tail-position are replaced by jumps to a
 *             label at the beginning of the method. As the JVM provides no way to
 *             jump from a method to another one, non-recursive calls in
 *             tail-position are not optimized.
 *             </p>
 *             <p>
 *             A method call is self-recursive if it calls the current method and
 *             the method is final (otherwise, it could
 *             be a call to an overridden method in a subclass).
 *             Recursive calls on a different instance are optimized. Since 'this'
 *             is not a local variable it is added as a label parameter.
 *             </p>
 *             <p>
 *             This phase has been moved after erasure to allow the use of vars
 *             for the parameters combined with a `WhileDo` (upcoming change).
 *             This is also beneficial to support polymorphic tail-recursive
 *             calls.
 *             </p>
 *             <p>
 *             If a method contains self-recursive calls, a label is added to at
 *             the beginning of its body and the calls are replaced by jumps to
 *             that label.
 *             </p>
 *             <p>
 *             In scalac, if the method had type parameters, the call must contain
 *             the same parameters as type arguments. This is no longer the case in
 *             dotc thanks to being located after erasure.
 *             In scalac, this is named tailCall but it does only provide optimization for
 *             self recursive functions, that's why it's renamed to tailrec
 *             </p>
 */
class TailRec extends MiniPhase {
  import TailRec._

  import dotty.tools.dotc.ast.tpd._

  override def phaseName: String = TailRec.name

  override def runsAfter = Set(Erasure.name) // tailrec assumes erased types

  final val labelFlags = Flags.Synthetic | Flags.Label | Flags.Method

  private def mkLabel(method: Symbol)(implicit ctx: Context): TermSymbol = {
    val name = TailLabelName.fresh()

    if (method.owner.isClass) {
      val MethodTpe(paramNames, paramInfos, resultType) = method.info

      val enclosingClass = method.enclosingClass.asClass
      val thisParamType =
        if (enclosingClass.is(Flags.Module)) enclosingClass.thisType
        else enclosingClass.classInfo.selfType

      ctx.newSymbol(method, name.toTermName, labelFlags,
        MethodType(nme.SELF :: paramNames, thisParamType :: paramInfos, resultType))
    }
    else ctx.newSymbol(method, name.toTermName, labelFlags, method.info)
  }

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context): tpd.Tree = {
    val sym = tree.symbol
    tree match {
      case dd@DefDef(name, Nil, vparams :: Nil, tpt, _)
        if (sym.isEffectivelyFinal) && !((sym is Flags.Accessor) || (dd.rhs eq EmptyTree) || (sym is Flags.Label)) =>
        val mandatory = sym.hasAnnotation(defn.TailrecAnnot)
        cpy.DefDef(dd)(rhs = {
          val defIsTopLevel = sym.owner.isClass
          val origMeth = sym
          val label = mkLabel(sym)
          val owner = ctx.owner.enclosingClass.asClass

          var rewrote = false

          // Note: this can be split in two separate transforms(in different groups),
          // than first one will collect info about which transformations and rewritings should be applied
          // and second one will actually apply,
          // now this speculatively transforms tree and throws away result in many cases
          val rhsSemiTransformed = {
            val transformer = new TailRecElimination(origMeth, owner, mandatory, label)
            val rhs = transformer.transform(dd.rhs)
            rewrote = transformer.rewrote
            rhs
          }

          if (rewrote) {
            if (tree.symbol.owner.isClass) {
              val classSym = tree.symbol.owner.asClass

              val labelDef = DefDef(label, vrefss => {
                assert(vrefss.size == 1, vrefss)
                val vrefs = vrefss.head
                val thisRef = vrefs.head
                val origMeth = tree.symbol
                val origVParams = vparams.map(_.symbol)
                new TreeTypeMap(
                  typeMap = identity(_)
                    .substThisUnlessStatic(classSym, thisRef.tpe)
                    .subst(origVParams, vrefs.tail.map(_.tpe)),
                  treeMap = {
                    case tree: This if tree.symbol == classSym => thisRef
                    case tree => tree
                  },
                  oldOwners = origMeth :: Nil,
                  newOwners = label :: Nil
                ).transform(rhsSemiTransformed)
              })
              val callIntoLabel = ref(label).appliedToArgs(This(classSym) :: vparams.map(x => ref(x.symbol)))
              Block(List(labelDef), callIntoLabel)
            } else { // inner method. Tail recursion does not change `this`
              val labelDef = DefDef(label, vrefss => {
                assert(vrefss.size == 1, vrefss)
                val vrefs = vrefss.head
                val origMeth = tree.symbol
                val origVParams = vparams.map(_.symbol)
                new TreeTypeMap(
                  typeMap = identity(_)
                    .subst(origVParams, vrefs.map(_.tpe)),
                  oldOwners = origMeth :: Nil,
                  newOwners = label :: Nil
                ).transform(rhsSemiTransformed)
              })
              val callIntoLabel = ref(label).appliedToArgs(vparams.map(x => ref(x.symbol)))
              Block(List(labelDef), callIntoLabel)
          }} else {
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

  class TailRecElimination(method: Symbol, enclosingClass: Symbol, isMandatory: Boolean, label: Symbol) extends tpd.TreeMap {

    import dotty.tools.dotc.ast.tpd._

    var rewrote = false

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
            def receiver =
              if (prefix eq EmptyTree) This(enclosingClass.asClass)
              else noTailTransform(prefix)

            val argumentsWithReceiver =
              if (this.method.owner.isClass) receiver :: arguments
              else arguments

            tpd.cpy.Apply(tree)(ref(label), argumentsWithReceiver)
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

        case t @ DefDef(_, _, _, _, _) =>
          t // todo: could improve to handle DefDef's with a label flag calls to which are in tail position

        case ValDef(_, _, _) | EmptyTree | Super(_, _) | This(_) |
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
  val name = "tailrec"

  final class TailContext(val tailPos: Boolean) extends AnyVal

  final val noTailContext = new TailContext(false)
  final val yesTailContext = new TailContext(true)
}
