import dotty.unused2

object Test {

  def main(args: Array[String]): Unit = {
    (
      (x: Int @unused2) => {
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
