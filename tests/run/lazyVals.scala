object Test {
 def foo = {
   lazy val s = 42 // needs LazyIntHolder
   s
  }

  def main(args: Array[String]): Unit = println(foo)
}
