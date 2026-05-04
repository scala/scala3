//> using options -Ycheck-termination -Yretain-trees

class C {
  import scala.annotation.{terminates}

  @terminates
  def sum(a: List[Int]): Int =
    var res = 0
    var l = a
    while (!l.isEmpty) {
      res += l.head
      l = l.tail
    }
    res

  @terminates
  def f(a: List[Int]): Int =
    var res = 0
    var l = a
    while (!l.isEmpty) {
      if true then l = l.tail
      else l = l.tail.tail
      res += f(l)
    }
    res
}

