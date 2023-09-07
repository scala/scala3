def Block(f: => Int): Int = f

def bar(): String =
  Block:
      2 + 2
    .toString

def foo(xs: List[Int]) =
  xs.map: x =>
      x + 1
    .filter: x =>
      x > 0
    println("foo")
