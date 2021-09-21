class Foo {
  type A = List[String]
  def foo(): A = Nil
}

object Test {
  def main(args: Array[String]): Unit = {
    println(classOf[Foo].getDeclaredMethod("foo").getGenericReturnType)
  }
}
