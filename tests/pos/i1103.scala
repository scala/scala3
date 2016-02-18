class Sys[S]
class Foo[T <: Sys[T]] {
  val t: T = ???
  def foo[A <: Sys[A]](x: A = t) = x
}
