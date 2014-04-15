package dotty.tools.dotc
package transform

import TreeTransforms._
import core.DenotTransformers._
import core.Symbols._
import core.Contexts._
import core.Types._
import core.Flags._
import core.Decorators._
import core.StdNames.nme
import ast.Trees._

/** This phase eliminates ExprTypes `=> T` and PolyTypes over value types `[X]T`.
 *  They are expressed in terms of nullary method or function types. More precisely:
 *
 *  For types:
 *
 *      => T        ==>    () => T      if T is the type of a parameter
 *                  ==>    ()T          otherwise
 *      [X]T        ==>    [X]()T
 *
 *  For definitions:
 *
 *      def f: R    ==>    def f(): R
 *      def f[X]: R ==>    def f[X](): R
 *      (x: => T)   ==>    (x: () => T)
 *
 *  For terms:
 *
 *      f           ==>    f()         if f had type => T and is not a parameter
 *      x           ==>    x.apply()   if x is a parameter that had type => T
 *      e.apply()   ==>    e           if e.apply() is an argument to a call-by-name parameter
 *      expr        ==>    () => expr  if other expr is an argument to a call-by-name parameter
 *
 */
class Nullarify extends TreeTransform with InfoTransformer {
  import ast.tpd._

  override def name: String = "nullarify"

  override def runsAfterGroupsOf: Set[String] = Set("splitter")
    // assumes idents and selects have symbols; interferes with splitter distribution
    // that's why it's "after group".

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree =
    ctx.traceIndented(s"transforming ${tree.show} at phase ${ctx.phase}", show = true) {

    def transformArg(arg: Tree, formal: Type): Tree = formal match {
      case _: ExprType =>
        arg match {
          case Apply(Select(qual, nme.apply), Nil) if qual.tpe <:< defn.FunctionClass(0).typeRef => qual
          case _ =>
            val meth = ctx.newSymbol(ctx.owner, nme.ANON_FUN, Synthetic,
                MethodType(Nil, Nil, arg.tpe.widen))
            Closure(meth, _ => arg)
        }
      case _ =>
        arg
    }

    // Compute the method type tree had before this phase is run.
    // This is needed to find out which parameters are by-name.
    val funType = tree.fun.symbol.info match {
      case info: PolyType => info.resultType
      case info => info
    }
    def methType(info: Type, tree: Tree): Type = tree match {
      case Apply(fn, args) => methType(info.resultType, fn)
      case _ => info
    }
    val MethodType(_, formals) = methType(funType, tree.fun)

    val args1 = tree.args.zipWithConserve(formals)(transformArg)
    cpy.Apply(tree, tree.fun, args1) withType nullarify(tree.tpe)
  }

  /** Insert () or .apply() if the term refers to something that was converted to a
   *  nullary method. Also, transform its type.
   */
  def insertParens(tree: Tree)(implicit ctx: Context): Tree = {
    val tp1 = transformInfo(tree.tpe, tree.symbol)
    val tree1 = tree.withType(tp1)
    val origType = tree.tpe.widenSingleton
    def result(implicit ctx: Context) = {
      tp1.widen match {
        case MethodType(Nil, _) if origType.widenExpr.isInstanceOf[ValueType] =>
          Apply(tree1, Nil)
        case _ =>
          origType match {
            case _: ExprType => // it's a by-name parameter
              Apply(Select(tree1, defn.Function0_apply), Nil)
            case _ =>
              tree1
          }
      }
    }
    result(ctx.withPhase(ctx.phase.next))
  }

  override def transformIdent(tree: Ident)(implicit ctx: Context, info: TransformerInfo): Tree =
    insertParens(tree)

  override def transformSelect(tree: Select)(implicit ctx: Context, info: TransformerInfo): Tree =
    insertParens(tree)

  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context, info: TransformerInfo): Tree =
    insertParens(tree)

  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val DefDef(mods, name, tparams, vparamss, tpt, rhs) = tree
    val vparamss1 =
      if (vparamss.isEmpty) Nil :: Nil
      else vparamss nestedMap { vparam =>
        val tp = vparam.tpt.tpe
        val tp1 = nullarifyParam(tp)
        if (tp eq tp1) vparam
        else cpy.ValDef(vparam, vparam.mods, vparam.name, vparam.tpt.withType(tp1), vparam.rhs)
      }
    cpy.DefDef(tree, mods, name, tparams, vparamss1, tpt, rhs)
  }

  def nullarify(tp: Type)(implicit ctx: Context): Type = tp match {
    case ExprType(rt) =>
      MethodType(Nil, Nil, rt)
    case pt: PolyType =>
      val rt = pt.resultType match {
        case mt: MethodType => nullarify(mt)
        case rt => MethodType(Nil, Nil, rt)
      }
      pt.derivedPolyType(pt.paramNames, pt.paramBounds, rt)
    case mt: MethodType =>
      mt.derivedMethodType(mt.paramNames, mt.paramTypes mapConserve nullarifyParam,
          nullarify(mt.resultType))
    case _ =>
      tp
  }

  def nullarifyParam(tp: Type)(implicit ctx: Context) = tp match {
    case ExprType(rt) => defn.FunctionType(Nil, rt)
    case _ => tp
  }

  def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type =
    if (defn.typeTestsOrCasts contains sym) tp
    else if (sym is Param) nullarifyParam(tp)
    else nullarify(tp)
}