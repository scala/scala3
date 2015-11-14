package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.DenotTransformers.DenotTransformer
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.core._
import dotty.tools.dotc.transform.TailRec._
import dotty.tools.dotc.transform.TreeTransforms.{TransformerInfo, MiniPhaseTransform}

/**
 * A Tail Rec Transformer
 *
 * @author     Erik Stenman, Iulian Dragos,
 *             ported to dotty by Dmitry Petrashko
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

  import dotty.tools.dotc.ast.tpd._

  override def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation = ref

  override def phaseName: String = "tailrec"
  override def treeTransformPhase = thisTransform // TODO Make sure tailrec runs at next phase.

  final val labelPrefix = "tailLabel"
  final val labelFlags = Flags.Synthetic | Flags.Label

  private def mkLabel(method: Symbol, abstractOverClass: Boolean)(implicit c: Context): TermSymbol = {
    val name = c.freshName(labelPrefix)

    c.newSymbol(method, name.toTermName, labelFlags, fullyParameterizedType(method.info, method.enclosingClass.asClass, abstractOverClass))
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
              val transformer = new TailRecElimination(origMeth, owner, thisTpe, mandatory, label, abstractOverClass = defIsTopLevel)
              val rhs = atGroupEnd(transformer.transform(dd.rhs)(_))
              rewrote = transformer.rewrote
              rhs
            }

            if (rewrote) {
              val dummyDefDef = cpy.DefDef(tree)(rhs = rhsSemiTransformed)
              val res = fullyParameterizedDef(label, dummyDefDef, abstractOverClass = defIsTopLevel)
              val call = forwarder(label, dd, abstractOverClass = defIsTopLevel)
              Block(List(res), call)
            } else {
              if (mandatory)
                ctx.error("TailRec optimisation not applicable, method not tail recursive", dd.pos)
              dd.rhs
            }
          })
        }
      case d: DefDef if d.symbol.hasAnnotation(defn.TailrecAnnot) =>
        ctx.error("TailRec optimisation not applicable, method is neither private nor final so can be overridden", d.pos)
        d
      case d if d.symbol.hasAnnotation(defn.TailrecAnnot) =>
        ctx.error("TailRec optimisation not applicable, not a method", d.pos)
        d
      case _ => tree
    }

  }

  class TailRecElimination(method: Symbol, enclosingClass: Symbol, thisType: Type, isMandatory: Boolean, label: Symbol, abstractOverClass: Boolean) extends tpd.TreeMap {

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
      def rewriteApply(tree: Tree, sym: Symbol): Tree = {
        def receiverArgumentsAndSymbol(t: Tree, accArgs: List[List[Tree]] = Nil, accT: List[Tree] = Nil):
            (Tree, Tree, List[List[Tree]], List[Tree], Symbol) = t match {
          case TypeApply(fun, targs) if fun.symbol eq t.symbol => receiverArgumentsAndSymbol(fun, accArgs, targs)
          case Apply(fn, args) if fn.symbol == t.symbol => receiverArgumentsAndSymbol(fn, args :: accArgs, accT)
          case Select(qual, _) => (qual, t, accArgs, accT, t.symbol)
          case x: This => (x, x, accArgs, accT, x.symbol)
          case x: Ident if x.symbol eq method => (EmptyTree, x, accArgs, accT, x.symbol)
          case x => (x, x, accArgs, accT, x.symbol)
        }

        val (reciever, call, arguments, typeArguments, symbol) = receiverArgumentsAndSymbol(tree)
        val recv = noTailTransform(reciever)

        val targs = typeArguments.map(noTailTransform)
        val argumentss = arguments.map(noTailTransforms)

        val recvWiden = recv.tpe.widenDealias

        val receiverIsSame = enclosingClass.typeRef.widenDealias =:= recvWiden
        val receiverIsSuper = (method.name eq sym) && enclosingClass.typeRef.widen <:< recvWiden
        val receiverIsThis = recv.tpe =:= thisType || recv.tpe.widen =:= thisType

        val isRecursiveCall = (method eq sym)

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
          if (isMandatory) c.error(s"Cannot rewrite recursive call: $reason", tree.pos)
          else c.debuglog("Cannot rewrite recursive call at: " + tree.pos + " because: " + reason)
          continue
        }

        def rewriteTailCall(recv: Tree): Tree = {
          c.debuglog("Rewriting tail recursive call:  " + tree.pos)
          rewrote = true
          val receiver = noTailTransform(recv)

          val callTargs: List[tpd.Tree] =
            if (abstractOverClass) {
              val classTypeArgs = recv.tpe.baseTypeWithArgs(enclosingClass).argInfos
              targs ::: classTypeArgs.map(x => ref(x.typeSymbol))
            } else targs

          val method = Apply(if (callTargs.nonEmpty) TypeApply(Ident(label.termRef), callTargs) else Ident(label.termRef),
            List(receiver))

          val res =
          if (method.tpe.widen.isParameterless) method
          else argumentss.foldLeft(method) {
            (met, ar) => Apply(met, ar) // Dotty deviation no auto-detupling yet.
          }
          res
        }

        if (isRecursiveCall) {
          if (ctx.tailPos) {
            if (recv eq EmptyTree) rewriteTailCall(This(enclosingClass.asClass))
            else if (receiverIsSame || receiverIsThis) rewriteTailCall(recv)
            else fail("it changes type of 'this' on a polymorphic recursive call")
          }
          else fail(defaultReason)
        } else {
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

        case ValDef(_, _, _) | EmptyTree | Super(_, _) | This(_) |
             Literal(_) | TypeTree(_) | DefDef(_, _, _, _, _) | TypeDef(_, _) =>
          tree

        case Return(expr, from) =>
          tpd.cpy.Return(tree)(noTailTransform(expr), from)
        case t: DefDef =>
          t // todo: could improve to handle DefDef's with a label flag calls to which are in tail position
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
