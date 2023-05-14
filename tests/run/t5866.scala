class Foo(val i: Int) extends AnyVal {
  override def toString = s"Foo($i)"
}
object Test {
  def main(args: Array[String]): Unit = {
    val i: Int = null.asInstanceOf[Int]
    println(i)
    val f: Foo = null.asInstanceOf[Foo]
    println(f)
  }
}
