//> using options -Ycheck-termination -Yretain-trees

class C {
  import scala.annotation.terminates

  @terminates
  def sumM(l: List[Int]): Int =
    l match
      case x :: xs => x + sumM(xs)
      case Nil => 0

  @terminates
  def sumITE(l: List[Int]): Int =
    if l.isEmpty then 0
    else l.head + sumITE(l.tail)

  @terminates
  def sumSkipOne(l: List[Int]): Int =
    if l.isEmpty || l.tail.isEmpty then 0
    else l.head + sumSkipOne(l.tail.tail)

  @terminates
  def sumSkipOneVal(l: List[Int]): Int =
    if l.isEmpty || l.tail.isEmpty then 0
    else
      val t = l.tail
      l.head + sumSkipOne(t.tail)
}

