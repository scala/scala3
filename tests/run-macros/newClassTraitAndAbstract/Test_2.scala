@main def Test: Unit = {
  val (cls1, show1) = makeTrait("foo")
  println(cls1.getClass)
  println(show1)

  val (cls2, show2) = makeAbstractClass("bar")
  println(cls2.getClass)
  println(show2)
}
