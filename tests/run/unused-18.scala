import dotty.unused

object Test {

  def main(args: Array[String]): Unit = {
    (
     (x: Int @unused) => {
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
