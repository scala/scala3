trait S[T] extends (T => T) with
  def apply(x: T) = ???
  extension (x: T) def show: String

given S[Int] with
  extension (x: Int) def show = x.toString

val x = 10.show
