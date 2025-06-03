object Main{
  object Foo
  object Bar{ type Qux = Unit }


  implicit def conv(f: Foo.type): Bar.type = Bar

  def B: Bar.type = Bar

  def main(args: Array[String]): Unit = {
    val x: Foo.Qux = null // error
    val y: B.Qux = null
    println(x)
    println(y)
  }
}