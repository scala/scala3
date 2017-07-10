package dotty.tools.dotc
package transform.localopt

import core.Contexts.Context
import core.Symbols._
import core.Types._
import typer.ConstFold
import ast.Trees._
import Simplify.desugarIdent

/** Various constant folding.
 *
 *  - Starts/ends with the constant folding implemented in typer (ConstFold).
 *
 *  - Join branches if they are "similar"
 *
 *  - regularize arithmetic and boolean expressions to have constants on the
 *    left, ie. 6 * 2 * a * 5 => 60 * a
 *
 *  - (if) specific optimisation that propagate booleans, negation, and factor
 *    out (nested) if with equivalent branches wrt to isSimilar. For example:
 *      - if (b) exp else exp → b; exp
 *      - if (b1) e1 else if (b2) e1 else e2 → if (b1 || b2) e1 else e2
 *
 *  - Constant propagation over pattern matching.
 *
 *  @author DarkDimius, OlivierBlanvillain
 */
 class ConstantFold(val simplifyPhase: Simplify) extends Optimisation {
  import ast.tpd._

  def visitor(implicit ctx: Context) = NoVisitor
  def clear(): Unit = ()

  def transformer(implicit ctx: Context): Tree => Tree = { x => preEval(x) match {
    // TODO: include handling of isInstanceOf similar to one in IsInstanceOfEvaluator
    // TODO: include methods such as Int.int2double(see ./tests/pos/harmonize.scala)
    case If(cond1, thenp, elsep) if isSimilar(thenp, elsep) =>
      Block(cond1 :: Nil, thenp)

    case If(cond1, If(cond2, thenp2, elsep2), elsep1) if isSimilar(elsep1, elsep2) =>
      If(cond1.select(defn.Boolean_&&).appliedTo(cond2), thenp2, elsep1)

    case If(cond1, If(cond2, thenp2, elsep2), elsep1) if isSimilar(elsep1, thenp2) =>
      If(cond1.select(defn.Boolean_!).ensureApplied.select(defn.Boolean_||).appliedTo(cond2), elsep1, elsep2)

    case If(cond1, thenp1, If(cond2, thenp2, elsep2)) if isSimilar(thenp1, thenp2) =>
      If(cond1.select(defn.Boolean_||).appliedTo(cond2), thenp1, elsep2)

    case If(cond1, thenp1, If(cond2, thenp2, elsep2)) if isSimilar(thenp1, elsep2) =>
      If(cond1.select(defn.Boolean_||).appliedTo(cond2.select(defn.Boolean_!).ensureApplied), thenp1, thenp2)

    case If(t: Literal, thenp, elsep) =>
      if (t.const.booleanValue) thenp
      else elsep

    case ift @ If(cond, thenp: Literal, elsep: Literal)
      if isBool(ift.tpe) && thenp.const.booleanValue && !elsep.const.booleanValue =>
        cond

    //  the lower two are disabled, as it may make the isSimilar rule not apply for a nested structure of iffs.
    //  see the example below:
    //         (b1, b2) match {
    //           case (true, true) => true
    //           case (false, false) => true
    //           case _ => false
    //         }
    // case ift @ If(cond, thenp: Literal, elsep)
    //   if isBool(ift.tpe) && thenp.const.booleanValue =>
    //     if (thenp.const.booleanValue)
    //       cond.select(defn.Boolean_||).appliedTo(elsep)
    //     else // thenp is false, this tree is bigger then the original
    //       cond.select(defn.Boolean_!).ensureApplied.select(defn.Boolean_&&).appliedTo(elsep)
    // case ift @ If(cond, thenp, elsep :Literal) if
    //    isBool(ift.tpe) && !elsep.const.booleanValue =>
    //       cond.select(defn.Boolean_&&).appliedTo(elsep)
    //   the other case ins't handled intentionally. See previous case for explanation

    case If(t @ Select(recv, _), thenp, elsep) if t.symbol eq defn.Boolean_! =>
      If(recv, elsep, thenp)

    case If(t @ Apply(Select(recv, _), Nil), thenp, elsep) if t.symbol eq defn.Boolean_! =>
      If(recv, elsep, thenp)

    // TODO: similar trick for comparisons.
    // TODO: handle comparison with min\max values
    case Apply(meth1 @ Select(Apply(meth2 @ Select(rec, _), Nil), _), Nil)
      if meth1.symbol == defn.Boolean_! && meth2.symbol == defn.Boolean_! =>
        rec

    case meth1 @ Select(meth2 @ Select(rec, _), _)
      if meth1.symbol == defn.Boolean_! && meth2.symbol == defn.Boolean_! && !ctx.erasedTypes =>
        rec

    case t @ Apply(Select(lhs, _), List(rhs)) =>
      val sym = t.symbol
      (lhs, rhs) match {
        case (lhs, Literal(_)) if !lhs.isInstanceOf[Literal] && simplifyPhase.CommutativePrimitiveOperations.contains(sym) =>
          rhs.select(sym).appliedTo(lhs)

        case (l, _) if (sym == defn.Boolean_&&) && isConst(l.tpe) =>
          val const = asConst(l.tpe).value.booleanValue
          if (const) Block(lhs :: Nil, rhs)
          else l

        case (l, x: Literal) if sym == defn.Boolean_== && isBool(l.tpe) && isBool(x.tpe) =>
          if (x.const.booleanValue) l
          else l.select(defn.Boolean_!).ensureApplied

        case (l, x: Literal) if sym == defn.Boolean_!= && isBool(l.tpe) && isBool(x.tpe) =>
          if (!x.const.booleanValue) l
          else l.select(defn.Boolean_!).ensureApplied

        case (x: Literal, l) if sym == defn.Boolean_== && isBool(l.tpe) && isBool(x.tpe) =>
          if (x.const.booleanValue) l
          else l.select(defn.Boolean_!).ensureApplied

        case (x: Literal, l) if sym == defn.Boolean_!= && isBool(l.tpe) && isBool(x.tpe) =>
          if (!x.const.booleanValue) l
          else l.select(defn.Boolean_!).ensureApplied

        case (l: Literal, _) if (sym == defn.Boolean_||) && isConst(l.tpe)   =>
          val const = asConst(l.tpe).value.booleanValue
          if (l.const.booleanValue) l
          else Block(lhs :: Nil, rhs)

        // case (Literal(Constant(1)), _)    if sym == defn.Int_*  => rhs
        // case (Literal(Constant(0)), _)    if sym == defn.Int_+  => rhs
        // case (Literal(Constant(1L)), _)   if sym == defn.Long_* => rhs
        // case (Literal(Constant(0L)), _)   if sym == defn.Long_+ => rhs
        // // TODO: same for float, double, short
        // // TODO: empty string concat
        // // TODO: disctribute & reorder constants
        // // TODO: merge subsequent casts
        // case (_, Literal(Constant(1)))    if sym == defn.Int_/  => lhs
        // case (_, Literal(Constant(1L)))   if sym == defn.Long_/ => lhs
        // case (_, Literal(Constant(0)))    if sym == defn.Int_/  =>
        //   Block(List(lhs),
        //     ref(defn.throwMethod).appliedTo(New(defn.ArithmeticExceptionClass.typeRef, defn.ArithmeticExceptionClass_stringConstructor, Literal(Constant("/ by zero")) :: Nil)))
        // case (_, Literal(Constant(0L)))  if sym == defn.Long_/ =>
        //   Block(List(lhs),
        //     ref(defn.throwMethod).appliedTo(New(defn.ArithmeticExceptionClass.typeRef, defn.ArithmeticExceptionClass_stringConstructor, Literal(Constant("/ by zero")) :: Nil)))

        case _ => t
      }

    // This case can only be triggered when running Simplify before pattern matching:
    // case t: Match
    //   if t.selector.tpe.isInstanceOf[ConstantType] &&
    //      t.cases.forall { x =>
    //        x.pat.tpe.isInstanceOf[ConstantType] || (isWildcardArg(x.pat) && x.guard.isEmpty)
    //      } =>
    //   val selectorValue = t.selector.tpe.asInstanceOf[ConstantType].value
    //   val better = t.cases.find(x => isWildcardArg(x.pat) || (x.pat.tpe.asInstanceOf[ConstantType].value eq selectorValue))
    //   if (better.nonEmpty) better.get.body
    //   else t

    case t: Literal => t
    case t: CaseDef => t
    case t if !isPureExpr(t) => t
    case t =>
      val s = ConstFold.apply(t)
      if ((s ne null) && s.tpe.isInstanceOf[ConstantType]) {
        val constant = s.tpe.asInstanceOf[ConstantType].value
        Literal(constant)
      } else t
    }
  }

  def preEval(t: Tree)(implicit ctx: Context) = {
    if (t.isInstanceOf[Literal] || t.isInstanceOf[CaseDef] || !isPureExpr(t)) t
    else {
      val s = ConstFold.apply(t)
      if ((s ne null) && s.tpe.isInstanceOf[ConstantType]) {
        val constant = s.tpe.asInstanceOf[ConstantType].value
        Literal(constant)
      } else t
    }
  }

  def isSimilar(t1: Tree, t2: Tree)(implicit ctx: Context): Boolean = t1 match {
    case t1: Apply =>
      t2 match {
        case t2: Apply =>
          (t1.symbol == t2.symbol) &&
          (t1.args zip t2.args).forall(x => isSimilar(x._1, x._2)) &&
          isSimilar(t1.fun, t2.fun)
        case _ => false
      }
    case t1: Ident =>
      desugarIdent(t1) match {
        case Some(t) =>
          val t2i = t2 match {
            case t2: Ident => desugarIdent(t2).getOrElse(t2)
            case _ => t2
          }
          isSimilar(t, t2i)
        case None => t1.symbol eq t2.symbol
      }
    case t1: Select => t2 match {
      case t2: Select =>
        (t1.symbol eq t2.symbol) &&
        isSimilar(t1.qualifier, t2.qualifier)
      case t2: Ident => desugarIdent(t2) match {
        case Some(t2) => isSimilar(t1, t2)
        case None => false
      }
      case _ => false
    }
    case t1: Literal => t2 match {
      case t2: Literal =>
        t1.const.tag   == t2.const.tag &&
        t1.const.value == t2.const.value
      case _ => false
    }
    case _ => false
  }

  def isBool(tpe: Type)(implicit ctx: Context): Boolean       = tpe.derivesFrom(defn.BooleanClass)
  def isConst(tpe: Type)(implicit ctx: Context): Boolean      = tpe.isInstanceOf[ConstantType]
  def asConst(tpe: Type)(implicit ctx: Context): ConstantType = tpe.asInstanceOf[ConstantType]
}
