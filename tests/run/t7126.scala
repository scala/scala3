import language.*

// Currently typer infers a Nothing as a CC[T] to be Nothing[T], and isn't
// able to figure out that Nothing[Any] =:= Nothing. We've had a discussion
// that it should instead infer CC[T] to be type lambda T => Nothing to be
// kind-correct. #2439

object T7126 {
  type T = Any
  boom(???): Option[T] // SOE
  def boom[CC[U]](t : CC[T]): Option[CC[T]] = None

  // okay
  foo(???): Option[Any]
  def foo[CC[U]](t : CC[Any]): Option[CC[Any]] = None
}

object Test {
  def main(args: Array[String]): Unit = ()
}
