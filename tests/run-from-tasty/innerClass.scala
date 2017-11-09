package foo {

  class A {
    class B
  }

}

class Test
object Test {
  def main(args: Array[String]): Unit = {
    val a = new foo.A
    println(a.getClass.getName)
    println(new a.B().getClass.getName)
  }
}