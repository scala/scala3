class Foo:
  private val Bar1: Int = 1
  private[Foo] val Bar2: Int = 2
  protected val Bar3: Int = 3
  protected[Foo] val Bar4: Int = 5
  class Baz:
    private[Foo] val Bar5: Int = 5
    protected[Foo] val Bar6: Int = 6
end Foo

def foo = new Foo

object Main:
  def main(args: Array[String]): Unit =
    println(foo.Bar1) // error
    println(foo.Bar2) // error
    println(foo.Bar3) // error
    println(foo.Bar4) // error
    println(foo.Baz.Bar5) // error
    println(foo.Baz.Bar6) // error
  end main
end Main
