object Test {
  type Result[A] = Errors | A
  final case class Errors(msgs: Seq[String])

  implicit class RichRes[A](val res: Result[A]) extends AnyVal {
    def map[B](f: A => B): Result[B] = res match {
      case xs: Errors => xs
      case a: A => f(a)
    }
  }

  var foo: Result[String] = ???
  def f(str: String): Int = ???

  foo.map(f(_)) // error

}
