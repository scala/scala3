sealed trait Xa[T]
sealed trait Mu[T] extends Xa[T]
object Xa {
  // bad
  implicit def convertMu[X[x] <: Xa[x], A, B](implicit t: X[A] with Xa[A]): X[B] = t.asInstanceOf[X[B]]
  // good
//  implicit def convertMu[X[x] <: Xa[x], A, B](implicit t: X[A] with Mu[A]): X[B] = t.asInstanceOf[X[B]]
}
object Mu {
  implicit def mu: Mu[Int] = new Mu[Int] {}
}

object App extends App {
  def constrain(a: Mu[Long]): Unit = println(a)
  constrain(Xa.convertMu)  // error
}