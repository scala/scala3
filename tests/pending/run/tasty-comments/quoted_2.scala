import Macros._

object Test {

  /** Object with a docstring */
  object Obj

  /** Val with a docstring */
  val x: Null = null

  val y: Null = null // val without a docstring

  def main(args: Array[String]): Unit = {
    printComment(Obj)
    printComment(x)
    printComment(y)
  }
}
