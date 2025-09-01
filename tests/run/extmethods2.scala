object Test extends App {

  class TC

  given stringListOps: TC => Object {
    type T = List[String]
    extension (x: T) def foo(y: T) = (x ++ y, summon[TC])
    extension (x: T) def bar(y: Int) = (x(0)(y), summon[TC])
  }

  def test(using TC) = {
    assert(List("abc").foo(List("def"))._1 == List("abc", "def"))
    assert(List("abc").bar(2)._1 == 'c')
  }

  test(using TC())

  object A {
    extension [T](xs: List[T]) {
      def second: T = xs.tail.head
      def third: T = xs.tail.tail.head
      def concat(ys: List[T]) = xs ++ ys
    }
    extension [T](xs: List[T]) {
      def zipp[U](ys: List[U]): List[(T, U)] = xs.zip(ys)
    }
    extension (xs: List[Int]) {
      def prod = (1 /: xs)(_ * _)
    }
  }

  object B {
    import A.*
    val xs = List(1, 2, 3)
    assert(xs.second == 2)
    assert(xs.third == 3)
    assert(A.second[Int](xs) == 2)
    assert(A.third(xs) == 3)
    assert(xs.prod == 6)
    assert(xs.concat(xs).length == 6)
    assert(xs.zipp(xs).map(_ + _).prod == 36)
    assert(xs.zipp[Int](xs).map(_ + _).prod == 36)
  }
}

