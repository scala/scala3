case class Contra[-A](f: A => Int)

case class Covarify[+F <: ([A] =>> Any), +A](fa: F[A]) // error: covariant type A occurs in invariant position in type F[A] of value fa
case class Covarify2[+F[+X] <: ([A] =>> Any), +A](fa: F[Int][A]) // error: covariant type A occurs in invariant position in type F[A] of value fa
case class Covarify3[+F <: [X] =>> [A] =>> Any, +A](fa: F[Int][A]) // error: covariant type A occurs in invariant position in type F[A] of value fa

@main def main = {
  val x = Covarify[Contra, Int](Contra[Int](_ + 5))
  val y: Covarify[Contra, Any] = x
  println(y.fa.f("abc"))
}