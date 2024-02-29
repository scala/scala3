def time[A](f: => A): A =
  val start = System.nanoTime
  val res = f
  val elapsed = (System.nanoTime - start)
  res

case class Foo(data: Int)

object o:
  val foo = time(Foo(3))
  println(foo.data)
