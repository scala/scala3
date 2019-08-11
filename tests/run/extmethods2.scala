object Test extends App {

  class TC

  given StringListOps given TC {
    type T = List[String]
    def (x: T) foo (y: T) = (x ++ y, the[TC])
    def (x: T) bar (y: Int) = (x(0)(y), the[TC])
  }

  def test given TC = {
    assert(List("abc").foo(List("def"))._1 == List("abc", "def"))
    assert(List("abc").bar(2)._1 == 'c')
  }

  test given TC()

  object A {
    given ListOps[T](xs: List[T]) {
      def second: T = xs.tail.head
      def third: T = xs.tail.tail.head
      def concat(ys: List[T]) = xs ++ ys
      def zipp[U](ys: List[U]): List[(T, U)] = xs.zip(ys)
    }
    given (xs: List[Int]) {
      def prod = (1 /: xs)(_ * _)
    }


  }

  object B {
    import given A._
    val xs = List(1, 2, 3)
    assert(xs.second[Int] == 2)
    assert(xs.third == 3)
    assert(A.ListOps.second[Int](xs) == 2)
    assert(A.ListOps.third(xs) == 3)
    assert(xs.prod == 6)
    assert(xs.concat(xs).length == 6)
    assert(xs.zipp(xs).map(_ + _).prod == 36)
    assert(xs.zipp[Int, Int](xs).map(_ + _).prod == 36)
  }
}

