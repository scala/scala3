package dotty.tools.dotc
package transform.localopt

import core.TypeErasure
import core.Contexts.Context
import core.Symbols._
import core.Types._
import core.Flags._
import ast.Trees._
import Simplify._

/** Removes side effect free statements in blocks and Defdef.
 *  Flattens blocks (except Closure-blocks)
 *  Note: BoxedUnit currently messes up this phase when run after erasure
 *
 *  @author DarkDimius, OlivierBlanvillain
 */
class DropNoEffects(val simplifyPhase: Simplify) extends Optimisation {
  import ast.tpd._

  def visitor(implicit ctx: Context) = NoVisitor
  def clear(): Unit = ()

  def transformer(implicit ctx: Context): Tree => Tree = {
    // Remove empty blocks
    case Block(Nil, expr) => expr

    // Keep only side effect free statements in blocks
    case a: Block  =>
      val newStats0 = a.stats.mapConserve(keepOnlySideEffects)

      // Flatten nested blocks
      val newStats1 = if (newStats0 eq a.stats) newStats0 else newStats0.flatMap {
        case x: Block  => x.stats ::: List(x.expr)
        case EmptyTree => Nil
        case t => t :: Nil
      }
      val (newStats2, newExpr) = a.expr match {
        case Block(stats2, expr) => (newStats1 ++ stats2, expr)
        case _ => (newStats1, a.expr)
      }

      if (newStats2.nonEmpty)
        cpy.Block(a)(stats = newStats2, newExpr)
      else newExpr

    // Keep only side effect free statements unit returning functions
    case a: DefDef
      if a.symbol.info.finalResultType.derivesFrom(defn.UnitClass) &&
        !a.rhs.tpe.derivesFrom(defn.UnitClass)                     &&
        !a.rhs.tpe.derivesFrom(defn.NothingClass)                  =>

      def insertUnit(t: Tree) = {
        if (!t.tpe.derivesFrom(defn.UnitClass)) Block(t :: Nil, unitLiteral)
        else t
      }
      cpy.DefDef(a)(rhs = insertUnit(keepOnlySideEffects(a.rhs)), tpt = TypeTree(defn.UnitType))

    case t => t
  }

  def keepOnlySideEffects(t: Tree)(implicit ctx: Context): Tree = t match {
    case l: Literal =>
      EmptyTree

    case t: This =>
      EmptyTree

    case Typed(exp, tpe) =>
      keepOnlySideEffects(exp)

    // If is pure, propagade the simplification
    case t @ If(cond, thenp, elsep) =>
      val nthenp = keepOnlySideEffects(thenp)
      val nelsep = keepOnlySideEffects(elsep)
      if (thenp.isEmpty && elsep.isEmpty) keepOnlySideEffects(cond)
      else cpy.If(t)(
        thenp = nthenp.orElse(if (thenp.isInstanceOf[Literal]) thenp else unitLiteral),
        elsep = nelsep.orElse(if (elsep.isInstanceOf[Literal]) elsep else unitLiteral))

    // Accessing a field of a product
    case t @ Select(rec, _) if isImmutableAccessor(t) =>
      keepOnlySideEffects(rec)

    // !name.eq(nme.TYPE_) && // Keep the .TYPE added by ClassOf, would be needed for AfterErasure
    // Without is(JavaStatic), { System.out } becomes { System }, but "Java class can't be used as value"
    case s @ Select(qual, name) if !s.symbol.is(Mutable | Lazy | Method | JavaStatic) =>
      keepOnlySideEffects(qual)

    case Block(List(t: DefDef), s: Closure) =>
      EmptyTree

    case bl @ Block(stats, expr) =>
      val stats1 = stats.mapConserve(keepOnlySideEffects)
      val stats2 = if (stats1 ne stats) stats1.filter(_ ne EmptyTree) else stats1
      val expr2: Tree = expr match {
        case t: Literal if t.tpe.derivesFrom(defn.UnitClass) => expr
        case _ => keepOnlySideEffects(expr).orElse(unitLiteral)
      }
      cpy.Block(bl)(stats2, expr2)

    case t: Ident if !t.symbol.is(Method | Lazy) && !t.symbol.info.isInstanceOf[ExprType] || effectsDontEscape(t) =>
      desugarIdent(t) match {
        case Some(t) if !(t.qualifier.symbol.is(JavaDefined) && t.qualifier.symbol.is(Package)) => t
        case _ => EmptyTree
      }

    case app: Apply if app.fun.symbol.is(Label) && !app.tpe.finalResultType.derivesFrom(defn.UnitClass) =>
      // This is "the scary hack". It changes the return type to Unit, then
      // invalidates the denotation cache. Because this optimisation only
      // operates locally, this should be fine.
      val denot = app.fun.symbol.denot
      if (!denot.info.finalResultType.derivesFrom(defn.UnitClass)) {
        val newLabelType = app.symbol.info match {
          case mt: MethodType =>
            mt.derivedLambdaType(mt.paramNames, mt.paramInfos, defn.UnitType)
          case et: ExprType =>
            et.derivedExprType(defn.UnitType)
        }
        val newD = app.symbol.asSymDenotation.copySymDenotation(info = newLabelType)
        newD.installAfter(simplifyPhase)
      }

      ref(app.symbol).appliedToArgs(app.args)

    case t @ Apply(fun, _) if effectsDontEscape(t) =>
      def getArgsss(a: Tree): List[Tree] = a match {
        case a: Apply => getArgsss(a.fun) ::: a.args
        case _ => Nil
      }
      def getSel(t: Tree): Tree = {t match {
        case t: Apply => getSel(t.fun)
        case t: Select => t.qualifier
        case t: TypeApply => getSel(t.fun)
        case _ => t
      }}
      val args = getArgsss(t)
      val rec = getSel(t)
      val prefix = rec match {
        case t: New =>
          args.map(keepOnlySideEffects)
        case _ =>
          rec :: args.map(keepOnlySideEffects)
      }
      Block(prefix, unitLiteral)

    case t => t
  }

  val constructorWhiteList: Set[String] = Set(
    "scala.Tuple2",
    "scala.Tuple3",
    "scala.Tuple4",
    "scala.Tuple5",
    "scala.Tuple6",
    "scala.Tuple7",
    "scala.Tuple8",
    "scala.Tuple9",
    "scala.Tuple10",
    "scala.Tuple11",
    "scala.Tuple12",
    "scala.Tuple13",
    "scala.Tuple14",
    "scala.Tuple15",
    "scala.Tuple16",
    "scala.Tuple17",
    "scala.Tuple18",
    "scala.Tuple19",
    "scala.Tuple20",
    "scala.Tuple21",
    "scala.Tuple22",
    "scala.Some"
  )

  val moduleWhiteList: Set[String] =
    constructorWhiteList.map(x => x + "$")

  val methodsWhiteList: List[String] = List(
    "java.lang.Math.min",
    "java.lang.Math.max",
    "java.lang.Object.eq",
    "java.lang.Object.ne",
    "scala.Boolean.$amp$amp",
    "scala.runtime.BoxesRunTime.unboxToBoolean",
    "scala.runtime.BoxesRunTime.unboxToLong",
    "scala.runtime.BoxesRunTime.unboxToInt",
    "scala.runtime.BoxesRunTime.unboxToShort",
    "scala.runtime.BoxesRunTime.unboxToDouble",
    "scala.runtime.BoxesRunTime.unboxToChar",
    "scala.runtime.BoxesRunTime.unboxToFloat"
  )

  /** Does this tree has side effects? This is an approximation awaiting real purity analysis... */
  def effectsDontEscape(t: Tree)(implicit ctx: Context): Boolean = t match {
    case Apply(fun, _) if fun.symbol.isConstructor && constructorWhiteList.contains(fun.symbol.owner.fullName.toString) =>
      true
    case Apply(fun, _) if methodsWhiteList.contains(fun.symbol.fullName.toString) =>
      true
    case Ident(_) if t.symbol.is(Module) && (t.symbol.is(Synthetic) || moduleWhiteList.contains(t.symbol.fullName.toString)) =>
      true
    case _ =>
      false
  }
}
