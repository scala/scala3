package foo {

  class A

  class B extends A

}

object Test {
  def main(args: Array[String]): Unit = {
    println(new foo.A().getClass.getName)
    println(new foo.B().getClass.getName)
  }
}