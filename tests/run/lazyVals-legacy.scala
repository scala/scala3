object Test {
 def foo = {
   lazy val s = 42 
   s
  }

  lazy val bar = "bar"

  def main(args: Array[String]): Unit = 
    println(foo)
    println(bar)
}
