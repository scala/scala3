object Test {
  def main(args: Array[String]): Unit = {
    val objectB = classOf[Foo[Any]].getClasses
    val returnType = objectB(1).getDeclaredMethod("m").getGenericReturnType
    println(returnType)
  }
  class Foo[T1] {
    class A[T2]

    object B  {
      def m: A[B.type] = ???
    }
  }
}

