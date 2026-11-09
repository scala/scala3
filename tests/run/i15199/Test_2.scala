class ScalaChild extends Child_1

@main def Test(): Unit =
  val methods = classOf[ScalaChild].getDeclaredMethods()
  assert(methods.length == 0, methods.mkString(", "))
