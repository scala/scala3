object Test extends App {
  // warmup
  def f(x: Int)(y: Int) = y
  def f(x: Int)(y: String) = y.length
  assert(f(1)(2) == 2)
  assert(f(1)("two") == 3)

  def g[T](x: T)(y: Int) = y
  def g[T](x: T)(y: String) = y.length
  assert(g[Int](1)(2) == 2)
  assert(g[Int](1)("two") == 3)
  assert(g(1)(2) == 2)
  assert(g(1)("two") == 3)

  def h[T](x: T)(y: T)(z: Int) = z
  def h[T](x: T)(y: T)(z: String) = z.length
  assert(h[Int](1)(1)(2) == 2)
  assert(h[Int](1)(1)("two") == 3)
  assert(h(1)(1)(2) == 2)
  assert(h(1)(1)("two") == 3)

  // Test with extension methods in given object
  object test1 {

    extension (x: Int)
      def |+| (y: Int) = x + y
      def |+| (y: String) = x + y.length

    extension [T](xs: List[T])
      def +++ (ys: List[T]): List[T] = xs ++ ys ++ ys
      def +++ (ys: Iterator[T]): List[T] = xs ++ ys ++ ys

    assert((1 |+| 2) == 3)
    assert((1 |+| "2") == 2)

    val xs = List(1, 2)
    assert((xs +++ xs).length == 6)
    assert((xs +++ xs.iterator).length == 4, xs +++ xs.iterator)
  }
  test1

  // Test with imported extension methods
  object test2 {
    import test1.*

    assert((1 |+| 2) == 3)
    assert((1 |+| "2") == 2)

    val xs = List(1, 2)
    assert((xs +++ xs).length == 6)
    assert((xs +++ xs.iterator).length == 4, xs +++ xs.iterator)
  }
  test2

  // Test with given extension methods coming from base class
  object test3 {
    class Foo {
      extension (x: Int) def |+| (y: Int) = x + y
      extension (x: Int) def |+| (y: String) = x + y.length

      extension [T](xs: List[T]) def +++ (ys: List[T]): List[T] = xs ++ ys ++ ys
      extension [T](xs: List[T]) def +++ (ys: Iterator[T]): List[T] = xs ++ ys ++ ys
    }
    given Bar: Foo()

    assert((1 |+| 2) == 3)
    assert((1 |+| "2") == 2)

    val xs = List(1, 2)
    assert((xs +++ xs).length == 6)
    assert((xs +++ xs.iterator).length == 4, xs +++ xs.iterator)
  }
  test3

  // Test with extension methods coming from given alias
  object test4 {
    given test3.Foo = test3.Bar

    assert((1 |+| 2) == 3)
    assert((1 |+| "2") == 2)

    val xs = List(1, 2)
    assert((xs +++ xs).length == 6)
    assert((xs +++ xs.iterator).length == 4, xs +++ xs.iterator)
  }
  test4

  class C {
    def xx (x: Any) = 2
  }
  extension (c: C) def xx(x: Int) = 1

  val c = new C
  assert(c.xx(1) == 2)  // member method takes precedence

  object D {
    extension (x: Int) def yy(y: Int) = x + y
  }

  given AnyRef:
    extension (x: Int) {
      def yy (y: Int) = x - y
    }

  import D.*
  assert((1 yy 2) == 3)  // imported extension method takes precedence

  trait Rectangle {
    def a: Long
    def b: Long
  }

  case class GenericRectangle(a: Long, b: Long) extends Rectangle
  case class Square(a: Long) extends Rectangle {
    def b: Long = a
  }

  extension (rectangle: Rectangle) def area: Long = 0
  extension (square: Square) def area: Long = square.a * square.a
  val rectangles = List(GenericRectangle(2, 3), Square(5))
  val areas = rectangles.map(_.area)
  assert(areas.sum == 0)
}