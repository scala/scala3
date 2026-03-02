object ExtMethods:

  case class Circle(x: Double, y: Double, radius: Double)

  extension (c: Circle)
    def circumference: Double = c.radius * math.Pi * 2

  val circle = Circle(0, 0, 1)
  circle.circumference
  assert(circle.circumference == circumference(circle))

  extension (x: String) def < (y: String) = x.compareTo(y) < 0
  extension [Elem](x: Elem) def #: (xs: Seq[Elem]) = x +: xs
  extension (x: Number) infix def min (y: Number) = x

  assert("a" < "bb")
  val xs = 1 #: Vector(2, 3)
  val n = java.lang.Integer(2) min java.lang.Double(3.0)

  extension [T](xs: List[T])
    def second = xs.tail.head

  assert(second[Int](List(1, 2, 3)) == List(1, 2, 3).second)

  extension [T: Numeric](x: T)
    def + (y: T): T = summon[Numeric[T]].plus(x, y)

  extension [T](x: T)(using n: Numeric[T])
    def - (y: T): T = n.minus(x, y)

  extension (ss: Seq[String])

    def longestStrings: Seq[String] =
      val maxLength = ss.map(_.length).max
      ss.filter(_.length == maxLength)

    def longestString: String = longestStrings.head

  import math.Ordered.orderingToOrdered

  extension [T](xs: List[T])(using Ordering[T])
    def smallest(n: Int): List[T] = xs.sorted.take(n)
    def smallestIndices(n: Int): List[Int] =
      val limit = smallest(n).max
      xs.zipWithIndex.collect { case (x, i) if x <= limit => i }

  trait IntOps:
    extension (i: Int) def isZero: Boolean = i == 0

    extension (i: Int) def safeMod(x: Int): Option[Int] =
      // extension method defined in same scope IntOps
      if x.isZero then None
      else Some(i % x)
  end IntOps

  object IntOpsEx extends IntOps:
    extension (i: Int) def safeDiv(x: Int): Option[Int] =
      // extension method brought into scope via inheritance from IntOps
      if x.isZero then None
      else Some(i / x)

  trait SafeDiv:
    import IntOpsEx._ // brings safeDiv and safeMod into scope

    extension (i: Int) def divide(d: Int) : Option[(Int, Int)] =
        // extension methods imported and thus in scope
      (i.safeDiv(d), i.safeMod(d)) match
        case (Some(d), Some(r)) => Some((d, r))
        case _ => None
  end SafeDiv

  def test1 =
    given ops1: IntOps() // brings safeMod into scope
    1.safeMod(2)

  class Lst[T](xs: T*):
    private val elems = xs.toList
    def foldLeft[U](x: U)(op: (U, T) => U): U = elems.foldLeft(x)(op)
    def ++ (other: Lst[T]): Lst[T] = Lst((elems ++ other.elems)*)

  trait Ord[T]:
    extension (x: T) def less (y: T): Boolean
  object Ord:
    given Ord[Int]:
      extension (x: Int) def less (y: Int): Boolean = x < y
  end Ord

  object Lst:

    extension [T](xs: Lst[Lst[T]])
      def flatten: Lst[T] = xs.foldLeft(Lst())(_ ++ _)

    given ord: [T: Ord] => Ord[Lst[T]]:
      extension (xs: Lst[T])
        def less (ys: Lst[T]): Boolean = ???
  end Lst

  def test2 =
    val xss = Lst(Lst(1, 2), Lst(3, 4))
    val xs: Lst[Int] = xss.flatten

    summon[Ord[Lst[Lst[Int]]]]

    assert(Lst.ord[Lst[Int]].less(xss)(Lst(Lst(3))))
    assert(xss `less` Lst(Lst(3)))
    assert(xss.flatten `less` Lst(3))

  extension (s: String)
    def position(ch: Char, n: Int): Int =
      if n < s.length && s(n) != ch then position(ch, n + 1)
      else n

  object DoubleOps:
    extension (x: Double) def ** (exponent: Int): Double =
      require(exponent > 0)
      if exponent == 0 then 1 else x * (x ** (exponent - 1))

  import DoubleOps.**
  assert(2.0 ** 3 == **(2.0)(3))

end ExtMethods