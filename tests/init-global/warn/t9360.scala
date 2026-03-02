class BaseClass(s: String) {
  def print: Unit = ()
}

object Obj { // warn
  val s: String = "hello"

  object AObj extends BaseClass(s) // warn

  object BObj extends BaseClass(s)

  val list = List(AObj, BObj)

  def print = {
    println(list)
  }
}

object ObjectInit {
  def main(args: Array[String]) = {
    Obj.AObj.print
    Obj.BObj.print
    Obj.print
  }
}
