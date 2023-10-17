object Foo:
  private val Bar1: Int = 1
  private[Foo] val Bar2: Int = 2
  protected val Bar3: Int = 3
  protected[Foo] val Bar4: Int = 5
  object Baz:
    private[Foo] val Bar5: Int = 5
    protected[Foo] val Bar6: Int = 6
end Foo

object Main:
  def main(args: Array[String]): Unit =
    println(Foo.Bar1) // error
    println(Foo.Bar2) // error
    println(Foo.Bar3) // error
    println(Foo.Bar4) // error
    println(Foo.Baz.Bar5) // error
    println(Foo.Baz.Bar6) // error
  end main
end Main
