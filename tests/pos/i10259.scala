//> using options -Werror -deprecation -feature

trait S[T] extends (T => T):
  def apply(x: T) = ???
  extension (x: T) def show: String

given S[Int]:
  extension (x: Int) def show = x.toString

val x = 10.show
