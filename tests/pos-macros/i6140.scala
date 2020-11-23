import scala.quoted._
sealed trait Trait[T] {
  type t = T
}

object O {
  def fn[T:Type](t : Trait[T])(using Quotes): Type[T] = Type.of[t.t]
}
