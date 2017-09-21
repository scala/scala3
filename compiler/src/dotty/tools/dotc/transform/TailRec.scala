package dotty.tools.dotc
package transform

import ast.Trees._
import ast.{TreeTypeMap, tpd}
import core._
import Contexts.Context
import Decorators._
import DenotTransformers.DenotTransformer
import Denotations.SingleDenotation
import Symbols._
import Types._
import NameKinds.TailLabelName
import TreeTransforms.{MiniPhaseTransform, TransformerInfo}

/**
 * A Tail Rec Transformer
 * @author     Erik Stenman, Iulian Dragos,
 *             ported and heavily modified for dotty by Dmitry Petrashko
 * @version    1.1
 *
 *             What it does:
 *             <p>
 *             Finds method calls in tail-position and replaces them with jumps.
 *             A call is in a tail-position if it is the last instruction to be
 *             executed in the body of a method.  This is done by recursing over
 *             the trees that may contain calls in tail-position (trees that can't
 *             contain such calls are not transformed). However, they are not that
 *             many.
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
 *
 *             Recursive calls on a different instance
 *             are optimized. Since 'this' is not a local variable it s added as
 *             a label parameter.
 *             </p>
 *             <p>
 *             This phase has been moved before pattern matching to catch more
 *             of the common cases of tail recursive functions. This means that
 *             more cases should be taken into account (like nested function, and
 *             pattern cases).
 *             </p>
 *             <p>
 *             If a method contains self-recursive calls, a label is added to at
 *             the beginning of its body and the calls are replaced by jumps to
 *             that label.
 *             </p>
 *             <p>
 *
 *             In scalac, If the method had type parameters, the call must contain same
 *             parameters as type arguments. This is no longer case in dotc.
 *             In scalac, this is named tailCall but it does only provide optimization for
 *             self recursive functions, that's why it's renamed to tailrec
 *             </p>
 */
class TailRec extends MiniPhaseTransform with DenotTransformer with FullParameterization { thisTransform =>
  import TailRec._

  import dotty.tools.dotc.ast.tpd._

  override def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation = ref

  override def phaseName: String = "tailrec"
  override def treeTransformPhase = thisTransform // TODO Make sure tailrec runs at next phase.

  final val labelFlags = Flags.Synthetic | Flags.Label

  /** Symbols of methods that have @tailrec annotatios inside */
  private val methodsWithInnerAnnots = new collection.mutable.HashSet[Symbol]()

  override def transformUnit(tree: Tree)(implicit ctx: Context, info: TransformerInfo): Tree = {
    methodsWithInnerAnnots.clear()
    tree
  }

  override def transformTyped(tree: Typed)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (tree.tpt.tpe.hasAnnotation(defn.TailrecAnnot))
      methodsWithInnerAnnots += ctx.owner.enclosingMethod
    tree
  }

  private def mkLabel(method: Symbol, abstractOverClass: Boolean)(implicit ctx: Context): TermSymbol = {
    val name = TailLabelName.fresh()

    if (method.owner.isClass)
      ctx.newSymbol(method, name.toTermName, labelFlags, fullyParameterizedType(method.info, method.enclosingClass.asClass, abstractOverClass, liftThisType = false))
    else ctx.newSymbol(method, name.toTermName, labelFlags, method.info)
  }

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    val sym = tree.symbol
    tree match {
      case dd@DefDef(name, tparams, vparamss0, tpt, _)
        if (sym.isEffectivelyFinal) && !((sym is Flags.Accessor) || (dd.rhs eq EmptyTree) || (sym is Flags.Label)) =>
        val mandatory = sym.hasAnnotation(defn.TailrecAnnot)
        atGroupEnd { implicit ctx: Context =>

          cpy.DefDef(dd)(rhs = {

            val defIsTopLevel = sym.owner.isClass
            val origMeth = sym
            val label = mkLabel(sym, abstractOverClass = defIsTopLevel)
            val owner = ctx.owner.enclosingClass.asClass
            val thisTpe = owner.thisType.widen

            var rewrote = false

            // Note: this can be split in two separate transforms(in different groups),
            // than first one will collect info about which transformations and rewritings should be applied
            // and second one will actually apply,
            // now this speculatively transforms tree and throws away result in many cases
            val rhsSemiTransformed = {
              val transformer = new TailRecElimination(origMeth, dd.tparams, owner, thisTpe, mandatory, label, abstractOverClass = defIsTopLevel)
              val rhs = atGroupEnd(implicit ctx => transformer.transform(dd.rhs))
              rewrote = transformer.rewrote
              rhs
            }

            if (rewrote) {
              val dummyDefDef = cpy.DefDef(tree)(rhs = rhsSemiTransformed)
              if (tree.symbol.owner.isClass) {
                val labelDef = fullyParameterizedDef(label, dummyDefDef, abstractOverClass = defIsTopLevel)
                val call = forwarder(label, dd, abstractOverClass = defIsTopLevel, liftThisType = true)
                Block(List(labelDef), call)
              } else { // inner method. Tail recursion does not change `this`
                val labelDef = polyDefDef(label, trefs => vrefss => {
                  val origMeth = tree.symbol
                  val origTParams = tree.tparams.map(_.symbol)
                  val origVParams = tree.vparamss.flatten map (_.symbol)
                  new TreeTypeMap(
                    typeMap = identity(_)
                      .substDealias(origTParams, trefs)
                      .subst(origVParams, vrefss.flatten.map(_.tpe)),
                      oldOwners = origMeth :: Nil,
                    newOwners = label :: Nil
                  ).transform(rhsSemiTransformed)
                })
                val callIntoLabel = (
                    if (dd.tparams.isEmpty) ref(label)
                    else ref(label).appliedToTypes(dd.tparams.map(_.tpe))
                  ).appliedToArgss(vparamss0.map(_.map(x=> ref(x.symbol))))
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
        }
      case d: DefDef if d.symbol.hasAnnotation(defn.TailrecAnnot) || methodsWithInnerAnnots.contains(d.symbol) =>
        ctx.error("TailRec optimisation not applicable, method is neither private nor final so can be overridden", sym.pos)
        d
      case d if d.symbol.hasAnnotation(defn.TailrecAnnot) || methodsWithInnerAnnots.contains(d.symbol) =>
        ctx.error("TailRec optimisation not applicable, not a method", sym.pos)
        d
      case _ => tree
    }

  }

  class TailRecElimination(method: Symbol, methTparams: List[Tree], enclosingClass: Symbol, thisType: Type, isMandatory: Boolean, label: Symbol, abstractOverClass: Boolean) extends tpd.TreeMap {

    import dotty.tools.dotc.ast.tpd._

    var rewrote = false

    private val defaultReason = "it contains a recursive call not in tail position"

    private var ctx: TailContext = yesTailContext

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
      trees.map(noTailTransform).asInstanceOf[List[Tr]]

    override def transform(tree: Tree)(implicit c: Context): Tree = {
      /* A possibly polymorphic apply to be considered for tail call transformation. */
      def rewriteApply(tree: Tree, sym: Symbol, required: Boolean = false): Tree = {
        def receiverArgumentsAndSymbol(t: Tree, accArgs: List[List[Tree]] = Nil, accT: List[Tree] = Nil):
            (Tree, Tree, List[List[Tree]], List[Tree], Symbol) = t match {
          case TypeApply(fun, targs) if fun.symbol eq t.symbol => receiverArgumentsAndSymbol(fun, accArgs, targs)
          case Apply(fn, args) if fn.symbol == t.symbol => receiverArgumentsAndSymbol(fn, args :: accArgs, accT)
          case Select(qual, _) => (qual, t, accArgs, accT, t.symbol)
          case x: This => (x, x, accArgs, accT, x.symbol)
          case x: Ident if x.symbol eq method => (EmptyTree, x, accArgs, accT, x.symbol)
          case x => (x, x, accArgs, accT, x.symbol)
        }

        val (prefix, call, arguments, typeArguments, symbol) = receiverArgumentsAndSymbol(tree)
        val hasConformingTargs = (typeArguments zip methTparams).forall{x => x._1.tpe <:< x._2.tpe}

        val targs = typeArguments.map(noTailTransform)
        val argumentss = arguments.map(noTailTransforms)

        val isRecursiveCall = (method eq sym)
        val recvWiden = prefix.tpe.widenDealias


        def continue = {
          val method = noTailTransform(call)
          val methodWithTargs = if (targs.nonEmpty) TypeApply(method, targs) else method
          if (methodWithTargs.tpe.widen.isParameterless) methodWithTargs
          else argumentss.foldLeft(methodWithTargs) {
            // case (method, args) => Apply(method, args) // Dotty deviation no auto-detupling yet. Interesting that one can do it in Scala2!
            (method, args) => Apply(method, args)
          }
        }
        def fail(reason: String) = {
          if (isMandatory || required) c.error(s"Cannot rewrite recursive call: $reason", tree.pos)
          else c.debuglog("Cannot rewrite recursive call at: " + tree.pos + " because: " + reason)
          continue
        }



        if (isRecursiveCall) {
          if (ctx.tailPos) {
            val receiverIsSame = enclosingClass.appliedRef.widenDealias =:= recvWiden
            val receiverIsThis = prefix.tpe =:= thisType || prefix.tpe.widen =:= thisType

            def rewriteTailCall(recv: Tree): Tree = {
              c.debuglog("Rewriting tail recursive call:  " + tree.pos)
              rewrote = true
              val receiver = noTailTransform(recv)

              val callTargs: List[tpd.Tree] =
                if (abstractOverClass) {
                  val classTypeArgs = recv.tpe.baseType(enclosingClass).argInfos
                  targs ::: classTypeArgs.map(x => ref(x.typeSymbol))
                } else targs

              val method = if (callTargs.nonEmpty) TypeApply(Ident(label.termRef), callTargs) else Ident(label.termRef)
              val thisPassed =
                if (this.method.owner.isClass)
                  method.appliedTo(receiver.ensureConforms(method.tpe.widen.firstParamTypes.head))
                else method

              val res =
                if (thisPassed.tpe.widen.isParameterless) thisPassed
                else argumentss.foldLeft(thisPassed) {
                  (met, ar) => Apply(met, ar) // Dotty deviation no auto-detupling yet.
                }
              res
            }

            if (!hasConformingTargs) fail("it changes type arguments on a polymorphic recursive call")
            else {
              val recv = noTailTransform(prefix)
              if (recv eq EmptyTree) rewriteTailCall(This(enclosingClass.asClass))
              else if (receiverIsSame || receiverIsThis) rewriteTailCall(recv)
              else fail("it changes type of 'this' on a polymorphic recursive call")
            }
          }
          else fail(defaultReason)
        } else {
          val receiverIsSuper = (method.name eq sym) && enclosingClass.appliedRef.widen <:< recvWiden

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

        case Ident(qual) =>
          val sym = tree.symbol
          if (sym == method && ctx.tailPos) rewriteApply(tree, sym)
          else tree

        case tree: Select =>
          val sym = tree.symbol
          if (sym == method && ctx.tailPos) rewriteApply(tree, sym)
          else tpd.cpy.Select(tree)(noTailTransform(tree.qualifier), tree.name)

        case Apply(fun, args) =>
          val meth = fun.symbol
          if (meth == defn.Boolean_|| || meth == defn.Boolean_&&)
            tpd.cpy.Apply(tree)(fun, transform(args))
          else
            rewriteApply(tree, meth)

        case TypeApply(fun, targs) =>
          val meth = fun.symbol
          rewriteApply(tree, meth)

        case tree@Block(stats, expr) =>
          tpd.cpy.Block(tree)(
            noTailTransforms(stats),
            transform(expr)
          )
        case tree @ Typed(t: Apply, tpt) if tpt.tpe.hasAnnotation(defn.TailrecAnnot) =>
          tpd.Typed(rewriteApply(t, t.fun.symbol, required = true), tpt)
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

        case Return(expr, from) =>
          tpd.cpy.Return(tree)(noTailTransform(expr), from)

        case _ =>
          super.transform(tree)
      }

      res
    }
  }

  /** If references to original `target` from fully parameterized method `derived` should be
    * rewired to some fully parameterized method, that method symbol,
    * otherwise NoSymbol.
    */
  override protected def rewiredTarget(target: Symbol, derived: Symbol)(implicit ctx: Context): Symbol = NoSymbol
}

object TailRec {

  final class TailContext(val tailPos: Boolean) extends AnyVal

  final val noTailContext = new TailContext(false)
  final val yesTailContext = new TailContext(true)
}
