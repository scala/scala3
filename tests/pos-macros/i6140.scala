import scala.quoted._
sealed trait Trait[T] {
  type t = T
}

object O {
  def fn[T](using s: Scope)(t : Trait[T])(using s.Type[T]): s.Type[T] = '[t.t]
}
