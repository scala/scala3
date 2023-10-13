object Foo:
  private val Bar: Int = 3
end Foo

object Main:
  def main(args: Array[String]): Unit =
    println(Foo.Bar) // error
  end main
end Main
