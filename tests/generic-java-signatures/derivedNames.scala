import java.lang.reflect.ParameterizedType

object Test {
  def main(args: Array[String]): Unit = {
    val objectB = classOf[Foo[Any]].getClasses
    val returnType = objectB(1).getDeclaredMethod("m").getGenericReturnType.asInstanceOf[ParameterizedType]
    if (scala.util.Properties.isWin)
      assert(returnType.toString == "Test$Foo.Test$Foo$A<Test.Test$Foo<T1>.B$>")
    else
      assert(returnType.toString == "Test$Foo$A<Test$Foo<T1>$B$>")
  }
  class Foo[T1] {
    class A[T2]

    object B  {
      def m: A[B.type] = ???
    }
  }
}

