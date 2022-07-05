// scalac: -source:future

trait Foo[A]:
  def map[B](f: A => B): Foo[B] = ???

def baz: Foo[(Int, String)] = ???

@main def main =
  for (x, y) <- baz
  yield ()
