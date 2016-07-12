import scala.language.higherKinds

trait Base {
  type Rep[T]
}

trait BaseExp extends Base {
  type Rep[T] = Exp[T]
  case class Exp[T](v: T)
}

trait BaseStr extends Base {
  type Rep[T] = String
}

trait BaseDirect extends Base {
  type Rep[T] = T
}

trait Test1 {
  trait Monad[X] {
    def x: X
  }
  sealed abstract class Either[A,B]
  case class Left[A,B](x: A) extends Either[A,B] with Monad[A]
  case class Right[A,B](x: B) extends Either[A,B] with Monad[B]
  def flatMap[X,Y,M[X]<:Monad[X]](m: M[X], f: X => M[Y]): M[Y] = f(m.x)
  println(flatMap(Right(1), {x: Int => Right(x)}))
}
trait Test2 {
  trait Monad[X] {
    def x: X
  }
  sealed abstract class Either[A,B]
  case class Left[A,B](x: A) extends Either[A,B] with Monad[A]
  case class Right[A,B](x: B) extends Either[A,B] with Monad[B]
  def flatMap[X,Y,M[X]](m: M[X], f: X => M[Y]): M[Y]
  println(flatMap(Left(1), {x: Int => Left(x)}))
}
trait Test3 {
  def flatMap[X,Y,M[X]](m: M[X], f: X => M[Y]): M[Y]
  println(flatMap(Some(1), {x: Int => Some(x)}))
}
