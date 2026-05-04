//> using options -Ycheck-termination -Yretain-trees

class C {
  import scala.annotation.{terminates}

  @terminates
  def f(l: List[Int]): Int = {
    var res = 0
    while (!l.isEmpty) { // error
      res += l.head
    }
    res
  }

  @terminates
  def g(a: List[Int]): Int = {
    var res = 0
    var l = a
    while (!l.isEmpty) {
      l = l.tail
      res += g(l)
    }
    g(l) // error
  }

  @terminates
  def h(a: List[Int]): Int = {
    var res = 0
    var l = a
    while (!l.isEmpty) { // error
      if true then l = l.tail
      res += l.head
    }
    res
  }

}

