//> using options -Wunused:patvars -Werror

case class A(x: Int, y: Int)

object Main {
  for {
    a <- List.empty[A]
    A(x, y) = a
  } yield x + y
}
