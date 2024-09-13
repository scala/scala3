object Test {
  def foo1(x: Int, y: Int, z: Int) = println((x, y, z))
  def foo2(x: Int = 0, y: Int, z: Int) = println((x, y, z))
  def bar1(x: Int, ys: Int*) = println((x, ys))
  def bar2(x: Int = 0, ys: Int*) = println((x, ys))

  def main(args: Array[String]) = {
    foo1(1, y = 2, 3)
    foo2(1, y = 2, 3)
    foo1(y = 1, 2, x = 3) // error: positional after named
    foo2(y = 1, 2, x = 3) // error: positional after named
    foo1(y = 1, 2, z = 3) // error: positional after named
    foo2(y = 1, 2, z = 3) // error: positional after named
    foo1(y = 1, 2) // error: positional after named
    foo2(y = 1, 2) // error: positional after named

    bar1() // error: missing arg
    bar2()
    bar1(1)
    bar2(1)
    bar1(x = 1)
    bar2(x = 1)
    bar1(ys = 1) // error: missing arg
    bar2(ys = 1)
    bar1(1, 2)
    bar2(1, 2)
    bar1(1, ys = 2)
    bar2(1, ys = 2)
    bar1(x = 1, 2)
    bar2(x = 1, 2)
    bar1(x = 1, ys = 2)
    bar2(x = 1, ys = 2)
    bar1(ys = 1, x = 2)
    bar2(ys = 1, x = 2)
    bar1(1, 2, 3)
    bar2(1, 2, 3)
    bar1(1, ys = 2, 3)
    bar2(1, ys = 2, 3)
    bar1(x = 1, 2, 3)
    bar2(x = 1, 2, 3)
    bar1(x = 1, ys = 2, 3)
    bar2(x = 1, ys = 2, 3)
    bar1(x = 1, 2, ys = 3)  // error: parameter ys is already instantiated
    bar1(1, 2, ys = 3)      // error: parameter ys is already instantiated
    bar2(x = 1, 2, ys = 3)  // error: parameter ys is already instantiated
    bar1(ys = 1, 2, x = 3)  // error: positional after named
    bar2(ys = 1, 2, x = 3)  // error: positional after named
  }
}
