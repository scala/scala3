def test(): Unit =
{
  val x: Foo[mypkg.Bar[String]] = ???
  val y: mypkg.Bar[String] = ???

  y.retainAll("fd") // works
  x.f.retainAll("fd"); // error

}

class Foo[T](val f: T)
