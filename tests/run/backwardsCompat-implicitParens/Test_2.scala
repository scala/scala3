@main def Test =
  given Int = 1
  given String = "x"

  println(new Bar(""))
  println(Bar(""))
  println(Bar.foo)

  println(new Bat())
  println(Bat())
  println(Bat.foo)

  println(new Bax())
  println(Bax())
  println(Bax.foo)

  println(new Baz())
  println(Baz())
  println(Baz.foo)
