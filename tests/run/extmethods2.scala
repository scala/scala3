object Test extends App {

  class TC

  given stringListOps with TC as Object {
    type T = List[String]
    def (x: T).foo(y: T) = (x ++ y, summon[TC])
    def (x: T).bar(y: Int) = (x(0)(y), summon[TC])
  }

  def test with TC = {
    assert(List("abc").foo(List("def"))._1 == List("abc", "def"))
    assert(List("abc").bar(2)._1 == 'c')
  }

  test.with(TC())

  object A {
    extension listOps on [T](xs: List[T]) {
      def second: T = xs.tail.head
      def third: T = xs.tail.tail.head
      def concat(ys: List[T]) = xs ++ ys
    }
    extension polyListOps on [T, U](xs: List[T]) {
      def zipp(ys: List[U]): List[(T, U)] = xs.zip(ys)
    }
    extension on (xs: List[Int]) {
      def prod = (1 /: xs)(_ * _)
    }
  }

  object B {
    import A.{given _}
    val xs = List(1, 2, 3)
    assert(xs.second[Int] == 2)
    assert(xs.third == 3)
    assert(A.listOps.second[Int](xs) == 2)
    assert(A.listOps.third(xs) == 3)
    assert(xs.prod == 6)
    assert(xs.concat(xs).length == 6)
    assert(xs.zipp(xs).map(_ + _).prod == 36)
    assert(xs.zipp[Int, Int](xs).map(_ + _).prod == 36)
  }
}

