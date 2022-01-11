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

  def bar(erased i: Int): Unit = ()

  erased def foo0: Int
  erased def foo1(): Int
  erased def foo2[T]: Int
  erased def foo3[T](): Int

}
