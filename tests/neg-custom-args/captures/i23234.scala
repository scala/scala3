abstract class MyFun:
  def apply(x: Int): Int

object Test:
  val myFun: MyFun = (x: Int) ?=> x + 10 // error