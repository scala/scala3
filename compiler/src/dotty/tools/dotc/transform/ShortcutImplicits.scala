package dotty.tools.dotc
package transform

import MegaPhase._
import core.DenotTransformers.{DenotTransformer, IdentityDenotTransformer}
import core.Symbols._
import core.Contexts._
import core.Types._
import core.Flags._
import core.StdNames.nme
import core.NameKinds.DirectMethodName
import ast.Trees._
import ast.tpd
import util.Store

/** This phase optimizes code using implicit function types, by applying two rewrite rules.
 *  Let IF be the implicit function type
 *
 *      implicit Us => R
 *
 *  (1) A method definition
 *
 *      def m(xs: Ts): IF = implicit (ys: Us) => E
 *
 *  is expanded to two methods:
 *
 *      def m(xs: Ts): IF = implicit (ys: Us) => m$direct(xs)(ys)
 *      def m$direct(xs: Ts)(ys: Us): R = E
 *
 *  (and equivalently for methods with type parameters or a different number of value parameter lists).
 *  An abstract method definition
 *
 *     def m(xs: Ts): IF
 *
 *  is expanded to:
 *
 *     def m(xs: Ts): IF
 *     def m$direct(xs: Ts)(ys: Us): R
 *
 *  (2) A reference `qual.apply` where `qual` has implicit function type and
 *  `qual` refers to a method `m` is rewritten to a reference to `m$direct`,
 *  keeping the same type and value arguments as they are found in `qual`.
 *
 *  Note: The phase adds direct methods for all methods with IFT results that
 *        are defined in the transformed compilation unit, as well as for all
 *        methods that are referenced from inside the unit. It does NOT do an
 *        info transformer that adds these methods everywhere where an IFT returning
 *        method exists (including in separately compiled classes).
 *        Adding such an info transformer is impractical because it would mean
 *        that we have to force the types of all members of classes that are referenced.
 *        But not adding an info transformer can lead to inconsistencies in RefChecks.
 *        We solve that by ignoring direct methods in Refchecks.
 *        Another, related issue is bridge generation, where we also generate
 *        shortcut methods on the fly.
 */
class ShortcutImplicits extends MiniPhase with IdentityDenotTransformer { thisPhase =>
  import ShortcutImplicits._
  import tpd._

  override def phaseName: String = ShortcutImplicits.name

  override def changesMembers: Boolean = true  // the phase adds "direct" methods

  /** A map to cache mapping local methods to their direct counterparts.
   *  A fresh map is created for each unit.
   */
  private var DirectMeth: Store.Location[MutableSymbolMap[Symbol]] = _
  private def directMeth(implicit ctx: Context) = ctx.store(DirectMeth)

  override def initContext(ctx: FreshContext): Unit =
    DirectMeth = ctx.addLocation[MutableSymbolMap[Symbol]]()


  override def prepareForUnit(tree: Tree)(implicit ctx: Context): Context =
    ctx.fresh.updateStore(DirectMeth, newMutableSymbolMap[Symbol])


  /** The direct method `m$direct` that accompanies the given method `m`.
    *  Create one if it does not exist already.
    */
  private def directMethod(sym: Symbol)(implicit ctx: Context): Symbol =
    if (sym.owner.isClass) shortcutMethod(sym, thisPhase)
    else directMeth.getOrElseUpdate(sym, newShortcutMethod(sym))

  /** Transform `qual.apply` occurrences according to rewrite rule (2) above */
  override def transformSelect(tree: Select)(implicit ctx: Context): Tree =
    if (tree.name == nme.apply &&
        defn.isImplicitFunctionType(tree.qualifier.tpe.widen) &&
        needsImplicitShortcut(tree.qualifier.symbol)) {
      def directQual(tree: Tree): Tree = tree match {
        case Apply(fn, args)     => cpy.Apply(tree)(directQual(fn), args)
        case TypeApply(fn, args) => cpy.TypeApply(tree)(directQual(fn), args)
        case Block(stats, expr)  => cpy.Block(tree)(stats, directQual(expr))
        case tree: RefTree =>
          cpy.Ref(tree)(DirectMethodName(tree.name.asTermName))
            .withType(tree.tpe.asInstanceOf[NamedType].prefix.select(directMethod(tree.symbol)))
      }
      directQual(tree.qualifier)
    } else tree

  /** Transform methods with implicit function type result according to rewrite rule (1) above */
  override def transformDefDef(mdef: DefDef)(implicit ctx: Context): Tree = {
    val original = mdef.symbol
    if (needsImplicitShortcut(original)) {
      val direct = directMethod(original)

      // Move @tailrec to the direct method
      original.getAnnotation(defn.TailrecAnnot) match {
        case Some(annot) =>
          direct.addAnnotation(annot)
          original.removeAnnotation(defn.TailrecAnnot)
        case _ =>
      }

      def splitClosure(tree: Tree): (List[Type] => List[List[Tree]] => Tree, Tree) = tree match {
        case Block(Nil, expr) => splitClosure(expr)
        case Block((meth @ DefDef(nme.ANON_FUN, Nil, clparams :: Nil, _, _)) :: Nil, cl: Closure) =>
          val tparamSyms = mdef.tparams.map(_.symbol)
          val vparamSymss = mdef.vparamss.map(_.map(_.symbol))
          val clparamSyms = clparams.map(_.symbol)
          val remappedCore = (ts: List[Type]) => (prefss: List[List[Tree]]) =>
            meth.rhs
              .subst(tparamSyms ::: (vparamSymss.flatten ++ clparamSyms),
                      ts.map(_.typeSymbol) ::: prefss.flatten.map(_.symbol))
              .changeOwnerAfter(original, direct, thisPhase)
              .changeOwnerAfter(meth.symbol, direct, thisPhase)
          val forwarder = ref(direct)
            .appliedToTypeTrees(tparamSyms.map(ref(_)))
            .appliedToArgss(vparamSymss.map(_.map(ref(_))) :+ clparamSyms.map(ref(_)))
          val fwdClosure = cpy.Block(tree)(cpy.DefDef(meth)(rhs = forwarder) :: Nil, cl)
          (remappedCore, fwdClosure)
        case id: RefTree =>
          val SAMType(mt) = id.tpe.widen
          splitClosure(tpd.Lambda(mt, args => id.select(nme.apply).appliedToArgs(args))(ctx.withOwner(original)))
        case EmptyTree =>
          (_ => _ => EmptyTree, EmptyTree)
      }

      val (remappedCore, fwdClosure) = splitClosure(mdef.rhs)
      val originalDef = cpy.DefDef(mdef)(rhs = fwdClosure)
      val directDef = transformDefDef(polyDefDef(direct.asTerm, remappedCore))
      flatTree(List(originalDef, directDef))
    }
    else mdef
  }
}

object ShortcutImplicits {
  val name: String = "shortcutImplicits"

  /** If this option is true, we don't specialize symbols that are known to be only
   *  targets of monomorphic calls.
   *  The reason for this option is that benchmarks show that on the JVM for monomorphic dispatch
   *  scenarios inlining and escape analysis can often remove all calling overhead, so we might as
   *  well not duplicate the code. We need more experience to decide on the best setting of this option.
   */
  final val specializeMonoTargets = true

  /** Should `sym` get a ..$direct companion?
    *  This is the case if `sym` is a method with a non-nullary implicit function type as final result type.
    *  However if `specializeMonoTargets` is false, we exclude symbols that are known
    *  to be only targets of monomorphic calls because they are effectively
    *  final and don't override anything.
    */
  def needsImplicitShortcut(sym: Symbol)(implicit ctx: Context): Boolean =
    sym.is(Method, butNot = Accessor) &&
    defn.isImplicitFunctionType(sym.info.finalResultType) &&
    defn.functionArity(sym.info.finalResultType) > 0 &&
    !sym.isAnonymousFunction &&
    (specializeMonoTargets || !sym.isEffectivelyFinal || sym.allOverriddenSymbols.nonEmpty)

  /** @pre    The type's final result type is an implicit function type `implicit Ts => R`.
    * @return The type of the `apply` member of `implicit Ts => R`.
    */
  private def directInfo(info: Type)(implicit ctx: Context): Type = info match {
    case info: PolyType   => info.derivedLambdaType(resType = directInfo(info.resultType))
    case info: MethodType => info.derivedLambdaType(resType = directInfo(info.resultType))
    case info: ExprType   => directInfo(info.resultType)
    case info             => info.member(nme.apply).info
  }

  /** A new `m$direct` method to accompany the given method `m` */
  private def newShortcutMethod(sym: Symbol)(implicit ctx: Context): Symbol = {
    val direct = sym.copy(
      name = DirectMethodName(sym.name.asTermName).asInstanceOf[sym.ThisName],
      flags = sym.flags | Synthetic,
      info = directInfo(sym.info))
    // make flags conformant to RefChecks. Override is meaningless after RefChecks.
    if (direct.allOverriddenSymbols.isEmpty) direct.resetFlag(Override)
    direct
  }

  def shortcutMethod(sym: Symbol, phase: DenotTransformer)(implicit ctx: Context): Symbol =
    sym.owner.info.decl(DirectMethodName(sym.name.asTermName))
      .suchThat(_.info matches directInfo(sym.info)).symbol
      .orElse(newShortcutMethod(sym).enteredAfter(phase))

  def isImplicitShortcut(sym: Symbol)(implicit ctx: Context): Boolean = sym.name.is(DirectMethodName)
}


