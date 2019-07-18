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
}