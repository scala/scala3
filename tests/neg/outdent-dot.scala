def Block(f: => Int): Int = f

def bar(): String =
  Block:
      2 + 2
     .toString  // error

def foo(xs: List[Int]) =
  xs.map: x =>
      x + 1
   .filter: x =>  // error
      x > 0
    println("foo")  // error
