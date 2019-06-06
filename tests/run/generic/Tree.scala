package generic

import Shapes._

/** enum Tree[T] {
 *    case True extends Tree[Boolean]
 *    case False extends Tree[Boolean]
 *    case Zero extends Tree[Int]
 *    case Succ(n: Tree[Int]) extends Tree[Int]
 *    case Pred(n: Tree[Int]) extends Tree[Int]
 *    case IsZero(n: Tree[Int]) extends Tree[Boolean]
 *    case If(cond: Boolean, thenp: Tree[T], elsep: Tree[T])
 *  }
 */
sealed trait Tree[TR] extends Enum

object Tree {

  val True: Tree[Boolean] = new Tree[Boolean] {
    def ordinal = 0
    override def toString = "True"
  }
  implicit def TrueSingleton: Singleton[True.type] = new Singleton[True.type](True)

  val False: Tree[Boolean] = new Tree[Boolean] {
    def ordinal = 1
    override def toString = "False"
  }
  implicit def FalseSingleton: Singleton[False.type] = new Singleton[False.type](False)

  val Zero: Tree[Int] = new Tree[Int] {
    def ordinal = 2
    override def toString = "Zero"
  }
  implicit def ZeroSingleton: Singleton[Zero.type] = new Singleton[Zero.type](Zero)

  abstract case class Succ(n: Tree[Int]) extends Tree[Int] {
    def ordinal = 3
  }
  object Succ {
    def apply(x: Tree[Int]): Tree[Int] = new Succ(x) {}
    implicit def SuccShape: Succ `shaped` Tree[Int] = new (Succ `shaped` Tree[Int]) {
      def toShape(x: Succ) = x.n
      def fromShape(x: Tree[Int]) = new Succ(x) {}
    }
  }

  abstract case class Pred(n: Tree[Int]) extends Tree[Int] {
    def ordinal = 4
  }
  object Pred {
    def apply(x: Tree[Int]): Tree[Int] = new Pred(x) {}
    implicit def PredShape: Pred `shaped` Tree[Int] = new (Pred `shaped` Tree[Int]) {
      def toShape(x: Pred) = x.n
      def fromShape(x: Tree[Int]) = new Pred(x) {}
    }
  }

  abstract case class IsZero(n: Tree[Int]) extends Tree[Boolean] {
    def ordinal = 5
  }
  object IsZero {
    def apply(x: Tree[Int]): Tree[Boolean] = new IsZero(x) {}
    implicit def IsZeroShape: IsZero `shaped` Tree[Int] = new (IsZero `shaped` Tree[Int]) {
      def toShape(x: IsZero) = x.n
      def fromShape(x: Tree[Int]) = new IsZero(x) {}
    }
  }

  abstract case class If[T](cond: Tree[Boolean], thenp: Tree[T], elsep: Tree[T]) extends Tree[T] {
    def ordinal = 6
  }
  object If {
    def apply[T](cond: Tree[Boolean], thenp: Tree[T], elsep: Tree[T]): Tree[T] = new If(cond, thenp, elsep) {}
    type Shape[T] = Prod[Tree[Boolean], Prod[Tree[T], Tree[T]]]
    implicit def IfShape[T]: If[T] `shaped` Shape[T] =
      new (If[T] `shaped` Shape[T]) {
        def toShape(x: If[T]) = Prod(x.cond, Prod(x.thenp, x.elsep))
        def fromShape(x: Shape[T]) = new If(x.fst, x.snd.fst, x.snd.snd) {}
      }
  }

  type Shape[T] =
      Sum[
        Sum[
          Sum[True.type, False.type],
          Sum[Zero.type, Succ]],
        Sum[
          Sum[Pred, IsZero],
          If[T]]]

  implicit def TreeShape[TS]: Tree[TS] `unfolds` Shape[TS]
     = new (Tree[TS] `shaped` Shape[TS]) {
         def toShape(x: Tree[TS]) = x match {
           case True => Fst(Fst(Fst(True)))
           case False => Fst(Fst(Snd(False)))
           case Zero => Fst(Snd(Fst(Zero)))
           case x: Succ => Fst(Snd(Snd(x)))
           case x: Pred => Snd(Fst(Fst(x)))
           case x: IsZero => Snd(Fst(Snd(x)))
           case x: If[TS] => Snd(Snd(x))
         }
         def fromShape(x: Shape[TS]): Tree[TS] = x match {
           case Fst(Fst(Fst(_true))) => _true.asInstanceOf[Tree[TS]]
           case Fst(Fst(Snd(_false))) => _false.asInstanceOf[Tree[TS]]
           case Fst(Snd(Fst(zero))) => zero.asInstanceOf[Tree[TS]]
           case Fst(Snd(Snd(succ))) => succ.asInstanceOf[Tree[TS]]
           case Snd(Fst(Fst(pred))) => pred.asInstanceOf[Tree[TS]]
           case Snd(Fst(Snd(isZero))) => isZero.asInstanceOf[Tree[TS]]
           case Snd(Snd(_if)) => _if
         }
      }
}
