//> using options -Werror

object Main {
  type IXY = (Int, Int)

  extension (xy: IXY) {
    def map(f: Int => Int): (Int, Int) = (f(xy._1), f(xy._2))
  }

  def main(args: Array[String]): Unit = {
    val a = (0, 1)
    println(a)
  }
}
