trait B {
  def y: this.type = this
  def x: y.type = y
}

object Test:
  def main(args: Array[String]): Unit =
    classOf[B].getMethods.sortBy(_.getName).filter(_.getName.startsWith("x")).foreach(m => {
      println(m)
      println(m.toGenericString)
    })
