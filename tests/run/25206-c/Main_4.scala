@main def Test(): Unit =
  println(new Outer().go)
  println(new Outer2().go)
  val outer: Inner = new Outer
  val outer2: Inner = new Outer2
  println(outer.switch)
  println(outer2.switch)

