@MyJava_1(value = "MyScala1", typeA = MyJava_1.MyClassTypeA.B)
object MyScala {
  def a(mj: MyJava_1): Unit = {
    println("MyJava")
  }

  @MyJava_1(typeA = MyJava_1.MyClassTypeA.A)
  def b(): Int = 1

  @MyJava_1(value = "MyScala2", typeA = MyJava_1.MyClassTypeA.B)
  def c(): Int = 2
}

