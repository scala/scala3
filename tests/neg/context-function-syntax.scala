val test =
  (using x: Int) => x  // error // error // error

val f = () ?=> 23 // error
val g: ContextFunction0[Int] = ??? // error at typer for RHS not expanded
val h: () ?=> Int = ??? // error

object Example3 extends App {
  final case class Foo[A, B](run: () ?=> Int) // error
}
