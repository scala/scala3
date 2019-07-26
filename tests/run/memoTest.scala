object Test extends App {
  import compiletime.memo

  var opCache: Int | Null = null

  def foo(x: Int) = {
    if (opCache == null) opCache = x
    opCache.asInstanceOf[Int] + 1
  }

  def bar(x: Int) = memo(x * x) + 1

  assert(foo(1) + foo(2) == 4)
  assert(bar(1) + bar(2) == 4)

  trait T {
    def x: Int
    def y: Int = memo {
      def inner = memo {
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
    given as Context given (c: Context) = memo(f(c))
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