trait Foo:
  def g(f: Int => Int): Int = 1
  def g(using String)(f: Int => String): String = "2"

@main def Test =
  val m: Foo = ???
  given String = "foo"
  m.g(x => "2")
  m.g(using summon[String])(x => "2")