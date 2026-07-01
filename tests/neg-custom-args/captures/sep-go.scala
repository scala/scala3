import caps.*

class Matrix(nrows: Int, ncols: Int) extends Mutable

def multiply(a: Matrix, b: Matrix, c: Matrix^): Unit = ???

def race(x: Matrix^) =
  // `go` reads `x` internally and writes its parameter `y` (the exclusive
  // third argument of `multiply`). Passing `x` for `y` therefore aliases the
  // exclusive write with `go`'s captured read of `x`, a separation violation.
  def go(y: Matrix^) =
    multiply(x, x, y)

  val f = go
  f(x)   // error
  go(x)  // error
