case object Zero
case class Zero2()
case class One(x: Int)
case class Two(x: Int, y: Int)
case class `Encoded!Name`(x: Int)

object Test {
  def main(args: Array[String]): Unit = {
    // FIXME: case objects are not handled like Scala 2 currently, see #723
    //println(Zero)

    println(Zero2())
    println(One(1))
    println(Two(1, 2))
    println(`Encoded!Name`(3))
  }
}
