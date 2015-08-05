object Test {

  def foo(xs: List[Int]): Int = {
    xs.foreach(x => return x)
    0
  }

  def bar(xs: List[Int]): Int = {
    lazy val y = if (xs.isEmpty) return -1 else xs.head
    y
  }

  def baz(x: Int): Int =
    byName { return -2; 3 }

  def byName(x: => Int): Int = x

  def bam(): Int = { // no non-local return needed here
    val foo = {
      return -3
      3
    }
    foo
  }

  def main(args: Array[String]) = {
    assert(foo(List(1, 2, 3)) == 1)
    assert(bar(Nil) == -1)
    assert(baz(3) == -2)
    assert(bam() == -3)
  }
}
