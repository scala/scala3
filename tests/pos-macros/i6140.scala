import scala.quoted._
sealed trait Trait[T] {
  type t = T
}

object O {
  def fn[T: Staged](t : Trait[T])(using QuoteContext): Staged[T] = '[t.t]
}
