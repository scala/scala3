object Test {

  def main(args: Array[String]): Unit = {
    (
      ghost (x: Int) => {
       println("lambda")
       42
     }
    )(foo)
  }

  def foo = {
    println("foo")
    42
  }
}
