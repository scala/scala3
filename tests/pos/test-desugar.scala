import language.postfixOps
object desugar {

  // variables
  var x: Int = 2
  var y = x * x
  val list = List(1, 2, 3)

  { var z: Int = y }

  def foo0(first: Int, second: Int = 2, third: Int = 3) = first + second
  def foo1(first: Int, second: Int = 2)(third: Int = 3) = first + second
  def foo2(first: Int)(second: Int = 2)(third: Int = 3) = first + second

  object caseClasses { self =>
    trait List[+T] {
      def head: T
      def tail: List[T]
    }

    case class Cons[+T](val head: T, val tail: List[T]) extends List[T]

    object Cons {
      def apply[T](head: T): Cons[T] = apply(head, Nil)
    }

    case object Nil extends List[Nothing] {
      def head = throw new Error()
      def tail = throw new Error()
    }
  }

  object patDefs {

    import caseClasses.*

    val xs: List[Int] = Cons(1, Cons(2, Nil))

    val Cons(y, ys) = xs
    val Cons(z, _) = xs
    val Cons(_, _) = xs

    val (cons: Cons[Int]) = xs

    val x1, y1, z1: Int = 1
  }

  object Binops {

    x :: y :: Nil

    val x :: y :: Nil = list

  }

  object fors {

    for (x <- List(1, 2, 3)) yield 2
    for (x <- List(1, 2, 3) if x % 2 == 0) yield x * x
    for (x <- List(1, 2, 3); y <- 0 to x) yield x * y
    for (x <- List(1, 2, 3); y <- 0 to x; if x + y % 2 == 0) yield x * y
    for (x <- List(1, 2, 3); y = x * x; if x + y % 2 == 0) yield x * y
    for (x <- List(1, 2, 3); y = x * x; z = x * y; u <- 0 to y) yield x * y * z * u

    for (x <- List(1, 2, 3)) println(x)
    for (x <- List(1, 2, 3) if x % 2 == 0) println(x * x)
    for (x <- List(1, 2, 3); y <- 0 to x) println(x * y)
    for (x <- List(1, 2, 3); y <- 0 to x; if x + y % 2 == 0) println(x * y)
    for (x <- List(1, 2, 3); y = x * x; if x + y % 2 == 0) println(x * y)
    for (x <- List(1, 2, 3); y = x * x; z = x * y; u <- 0 to y) println(x * y * z * u)
  }

  object misc {
    Symbol("hello")
    s"this is a $x + ${x + y} string"
    type ~[X, Y] = Tuple2[X, Y]
    val pair: Int ~ String = 1 -> "abc"
    def foo(xs: Int*) = xs.length
    foo(list*)
    (list length)
    - desugar.x
    def bar(x: => Int) = x
    (x + y) + 1
    while (x < 10) x += 1
    while ({ x -= 1 ; x > 0 }) ()
  }

}
