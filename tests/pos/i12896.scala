trait IO[E] {
  def map[B](f: Any => B): IO[E] = ???
  def flatMap[C](f: Any => IO[C]): IO[E | C] = ???
}

class Test {
  def test: Unit = {
    val a: IO[Nothing] = ???

    val d = a.flatMap(y => a.flatMap(z => a.map(_ => z)))
  }
}