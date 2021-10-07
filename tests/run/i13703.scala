trait Foo extends reflect.Selectable

@main def Test: Unit =
  val f = new Foo { var i: Int = 0 }
  f.i = 1