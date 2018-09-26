package dotty.tools.dotc
package transform

import ast.Trees._
import ast.{TreeTypeMap, tpd}
import core._
import Flags._
import Contexts.Context
import Decorators._
import Symbols._
import StdNames.nme
import Types._
import config.Printers.tailrec
import NameKinds.TailLabelName
import MegaPhase.MiniPhase
import reporting.diagnostic.messages.TailrecNotApplicable

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
  import tpd._

  override def phaseName: String = TailRec.name

  override def runsAfter: Set[String] = Set(Erasure.name) // tailrec assumes erased types

  final val labelFlags: FlagSet = Synthetic | Label | Method

  private def mkLabel(method: Symbol)(implicit ctx: Context): TermSymbol = {
    val name = TailLabelName.fresh()

    if (method.owner.isClass) {
      val MethodTpe(paramNames, paramInfos, resultType) = method.info

      val enclosingClass = method.enclosingClass.asClass
      val thisParamType =
        if (enclosingClass.is(Module)) enclosingClass.thisType
        else enclosingClass.classInfo.selfType

      ctx.newSymbol(method, name.toTermName, labelFlags,
        MethodType(nme.SELF :: paramNames, thisParamType :: paramInfos, resultType))
    }
    else ctx.newSymbol(method, name.toTermName, labelFlags, method.info)
  }

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context): tpd.Tree = {
    val method = tree.symbol
    val mandatory = method.hasAnnotation(defn.TailrecAnnot)
    def noTailTransform = {
      // FIXME: want to report this error on `tree.namePos`, but
      // because of extension method getting a weird pos, it is
      // better to report on methodbol so there's no overlap
      if (mandatory)
        ctx.error(TailrecNotApplicable(method), method.pos)
      tree
    }

    val isCandidate = method.isEffectivelyFinal &&
      !((method is Accessor) || (tree.rhs eq EmptyTree) || (method is Label))

    if (isCandidate) {
      val label = mkLabel(method)

      // Note: this can be split in two separate transforms(in different groups),
      // than first one will collect info about which transformations and rewritings should be applied
      // and second one will actually apply,
      // now this speculatively transforms tree and throws away result in many cases
      val transformer = new TailRecElimination(method, mandatory, label)
      val rhsSemiTransformed = transformer.transform(tree.rhs)

      if (transformer.rewrote) {
        val DefDef(name, Nil, vparams :: Nil, _, _) = tree
        val origVParams = vparams.map(_.symbol)
        val origVParamRefs = origVParams.map(ref(_))

        if (method.owner.isClass) {
          val classSym = tree.symbol.owner.asClass

          val labelDef = DefDef(label, vrefss => {
            assert(vrefss.size == 1, vrefss)
            val vrefs = vrefss.head
            val thisRef = vrefs.head

            new TreeTypeMap(
              typeMap = identity(_)
                .substThisUnlessStatic(classSym, thisRef.tpe)
                .subst(origVParams, vrefs.tail.map(_.tpe)),
              treeMap = {
                case tree: This if tree.symbol == classSym => thisRef
                case tree => tree
              },
              oldOwners = method :: Nil,
              newOwners = label :: Nil
            ).transform(rhsSemiTransformed)
          })
          val callIntoLabel = ref(label).appliedToArgs(This(classSym) :: origVParamRefs)
          cpy.DefDef(tree)(rhs = Block(List(labelDef), callIntoLabel))
        } else {
          // Inner methods: Tail recursion does not change `this`
          val labelDef = DefDef(label, vrefss => {
            assert(vrefss.size == 1, vrefss)
            val vrefs = vrefss.head

            new TreeTypeMap(
              typeMap = identity(_)
                .subst(origVParams, vrefs.map(_.tpe)),
              oldOwners = method :: Nil,
              newOwners = label :: Nil
            ).transform(rhsSemiTransformed)
          })
          val callIntoLabel = ref(label).appliedToArgs(origVParamRefs)
          cpy.DefDef(tree)(rhs = Block(List(labelDef), callIntoLabel))
        }
      }
      else noTailTransform
    }
    else noTailTransform
  }

  private class TailRecElimination(method: Symbol, isMandatory: Boolean, label: Symbol) extends tpd.TreeMap {
    import tpd._

    var rewrote: Boolean = false

    /** Symbols of Labeled blocks that are in tail position. */
    private val tailPositionLabeledSyms = new collection.mutable.HashSet[Symbol]()

    private[this] var inTailPosition = true

    /** Rewrite this tree to contain no tail recursive calls */
    def transform(tree: Tree, tailPosition: Boolean)(implicit ctx: Context): Tree = {
      if (inTailPosition == tailPosition) transform(tree)
      else {
        val saved = inTailPosition
        inTailPosition = tailPosition
        try transform(tree)
        finally inTailPosition = saved
      }
    }

    def yesTailTransform(tree: Tree)(implicit ctx: Context): Tree =
      transform(tree, tailPosition = true)

    def noTailTransform(tree: Tree)(implicit ctx: Context): Tree =
      transform(tree, tailPosition = false)

    def noTailTransforms[Tr <: Tree](trees: List[Tr])(implicit ctx: Context): List[Tr] =
      trees.mapConserve(noTailTransform).asInstanceOf[List[Tr]]

    override def transform(tree: Tree)(implicit ctx: Context): Tree = {
      /* Rewrite an Apply to be considered for tail call transformation. */
      def rewriteApply(tree: Apply): Tree = {
        val arguments = noTailTransforms(tree.args)

        def continue =
          cpy.Apply(tree)(noTailTransform(tree.fun), arguments)

        def fail(reason: String) = {
          if (isMandatory) ctx.error(s"Cannot rewrite recursive call: $reason", tree.pos)
          else tailrec.println("Cannot rewrite recursive call at: " + tree.pos + " because: " + reason)
          continue
        }

        def enclosingClass = method.enclosingClass.asClass

        val call = tree.fun.symbol
        val prefix = tree.fun match {
          case Select(qual, _) => qual
          case x: Ident if x.symbol eq method => EmptyTree
          case x => x
        }
        val isRecursiveCall = call eq method

        if (isRecursiveCall) {
          if (inTailPosition) {
            tailrec.println("Rewriting tail recursive call: " + tree.pos)
            rewrote = true

            def receiver =
              if (prefix eq EmptyTree) This(enclosingClass)
              else noTailTransform(prefix)

            val argumentsWithReceiver =
              if (method.owner.isClass) receiver :: arguments
              else arguments

            cpy.Apply(tree)(ref(label), argumentsWithReceiver)
          }
          else fail("it is not in tail position")
        } else {
          // FIXME `(method.name eq call)` is always false (Name vs Symbol). What is this trying to do?
          val receiverIsSuper = (method.name eq call) && enclosingClass.appliedRef.widen <:< prefix.tpe.widenDealias

          if (receiverIsSuper) fail("it contains a recursive call targeting a supertype")
          else continue
        }
      }

      def rewriteTry(tree: Try): Try = {
        val expr = noTailTransform(tree.expr)
        val hasFinalizer = tree.finalizer ne EmptyTree
        // SI-1672 Catches are in tail position when there is no finalizer
        val cases =
          if (hasFinalizer) noTailTransforms(tree.cases)
          else transformSub(tree.cases)
        val finalizer =
          if (hasFinalizer) noTailTransform(tree.finalizer)
          else EmptyTree
        cpy.Try(tree)(expr, cases, finalizer)
      }

      tree match {
        case tree@Apply(fun, args) =>
          val meth = fun.symbol
          if (meth == defn.Boolean_|| || meth == defn.Boolean_&&)
            cpy.Apply(tree)(noTailTransform(fun), transform(args))
          else
            rewriteApply(tree)

        case tree: Select =>
          cpy.Select(tree)(noTailTransform(tree.qualifier), tree.name)

        case tree@Block(stats, expr) =>
          cpy.Block(tree)(
            noTailTransforms(stats),
            transform(expr)
          )

        case tree@If(cond, thenp, elsep) =>
          cpy.If(tree)(
            noTailTransform(cond),
            transform(thenp),
            transform(elsep)
          )

        case tree@CaseDef(_, _, body) =>
          cpy.CaseDef(tree)(body = transform(body))

        case tree@Match(selector, cases) =>
          cpy.Match(tree)(
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
          if (inTailPosition)
            tailPositionLabeledSyms += bind.symbol
          cpy.Labeled(tree)(bind, transform(expr))

        case Return(expr, from) =>
          val fromSym = from.symbol
          val inTailPosition = fromSym.is(Label) && tailPositionLabeledSyms.contains(fromSym)
          cpy.Return(tree)(transform(expr, inTailPosition), from)

        case _ =>
          super.transform(tree)
      }
    }
  }
}

object TailRec {
  val name: String = "tailrec"
}
