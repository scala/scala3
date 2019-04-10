object Test {

  def main(args: Array[String]): Unit = {

    bar({
      println("Test0")
      Test
    }.foo0)

    bar({
      println("Test1")
      Test
    }.foo1())

    bar({
      println("Test2")
      Test
    }.foo2[Int])

    bar({
      println("Test3")
      Test
    }.foo3[Int]())

    ()
  }

  def bar erased (i: Int): Unit = ()

  erased def foo0: Int = 0
  erased def foo1(): Int = 1
  erased def foo2[T]: Int = 2
  erased def foo3[T](): Int = 3

}
