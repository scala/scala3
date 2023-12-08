
class Foo:
  val generic: List[String] = ???

@main def Test =
  val tpe = classOf[Foo].getDeclaredField("generic").getGenericType()
  assert(tpe.getTypeName == "scala.collection.immutable.List<java.lang.String>")

  val tpe2 = classOf[Foo].getDeclaredMethod("generic").getGenericReturnType()
  assert(tpe2.getTypeName == "scala.collection.immutable.List<java.lang.String>")
