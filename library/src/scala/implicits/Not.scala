package scala.implicits

final class Not[+T] private ()

object Not {
  def value: Not[Nothing] = new Not[Nothing]()
  implicit def amb1[T](implicit ev: T): Not[T] = ???
  implicit def amb2[T](implicit ev: T): Not[T] = ???
}
