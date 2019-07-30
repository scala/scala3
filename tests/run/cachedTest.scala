object Test extends App {
  import annotation.meta.cached

  @cached def bar given(x: Int) = x * x + 1

  assert((bar given 1) + (bar given 2) == 4)

  trait T {
    def x: Int
    @cached def y: Int = {
      @cached def inner = {
        println("computing inner");
        x * x
      }
      inner + inner
    }
  }
  val t = new T {
    def x = 3
    assert(y == 18)
  }
  assert(t.y == 18)

  class Context(val n: Int)
  def f(c: Context): Context = {
    println("computing f")
    Context(c.n + 1)
  }
  given as Context(0)

  locally {
    @cached given as Context given (c: Context) = f(c)
    println(the[Context].n)
    println(the[Context].n)
  }

  val ctx = f(the[Context])
  locally {
    given as Context = ctx
    println(the[Context].n)
    println(the[Context].n)
  }
}