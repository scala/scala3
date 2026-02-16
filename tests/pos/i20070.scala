
trait F[A]
given F[Int] = new F {}
def f[A: F] = { (x: A) => x }

@main def test =
  println(f[Int](1))
