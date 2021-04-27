class Foo[T]
object Test {
  class Inner extends Foo[String]

  def main(args: Array[String]): Unit = {
    class Local extends Foo[String]

    println((new Inner).getClass.getGenericSuperclass) // Foo<java.lang.String>

    println((new Local).getClass.getGenericSuperclass) // Foo<java.lang.String>
  }
}
