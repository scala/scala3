object Test {
  def main(args: Array[String]): Unit = {
    println(Obj.myFinalVar)
    Obj.myFinalVar = true
    println(Obj.myFinalVar)

    val o = new Cls
    println(o.myFinalVar)
    o.myFinalVar = true
    println(o.myFinalVar)
  }
}

object Obj {
  final var myFinalVar: Boolean = false
}

class Cls {
  final var myFinalVar: Boolean = false
}
