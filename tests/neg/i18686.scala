object Foo:
  private val Bar1: Int = 3
  private[Foo] val Bar2: Int = 3
  protected val Bar3: Int = 3
end Foo

object Main:
  def main(args: Array[String]): Unit =
    println(Foo.Bar1) // error
    println(Foo.Bar2) // error
    println(Foo.Bar3) // error
  end main
end Main
