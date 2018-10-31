object Test {

  def main(args: Array[String]): Unit = {
    val x = new Outer
    val y = new Outer
    val i = new x.Inner
    val j = new y.Inner
    i match {
      case _: y.Inner => assert(false)
      case _: x.Inner => // OK
    }
  }

}
