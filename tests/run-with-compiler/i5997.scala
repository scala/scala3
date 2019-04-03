object Test {
  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
    val v = '{ (if true then Some(1) else None).map(v => v+1) }
    println(v.show)
  }
}
