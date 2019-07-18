object Test extends App {

  var opCache: Int | Null = null

  def foo(x: Int) = {
    if (opCache == null) opCache = x
    opCache.asInstanceOf[Int] + 1
  }

  assert(foo(1) + foo(2) == 4)

}