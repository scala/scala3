sealed trait Expr[+A] extends ExprTuple[A]

case class Lit[A](value: A) extends Expr[A]

trait ExprTuple[+A] {
  def _1(implicit ev: ExprTuple.Has2[A]): Expr[ev._1] = ???
  def _2(implicit ev: ExprTuple.Has2[A]): Expr[ev._2] = ???
}

object ExprTuple {
  trait Has2[-A] { type _1; type _2 }
  object Has2 {
    type Aux[-A, A1, A2] = Has2[A] { type _1 = A1; type _2 = A2 }
  }

  implicit def tuple2[A, B]: Has2.Aux[(A, B), A, B] = ???
}