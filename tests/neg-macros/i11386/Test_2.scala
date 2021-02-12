import test._

object Main {

  @main def dottyTest(): Unit = {
    dummy(0) // error
    dummy(1)
    dummy(int2String(0)) // error
    dummy(int2String(1))
  }

  def dummy(msg: String): Unit = println(msg) //A simple dummy method to call implicitly test.int2String
}
