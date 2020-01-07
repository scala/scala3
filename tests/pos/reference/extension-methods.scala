object ExtMethods with

  case class Circle(x: Double, y: Double, radius: Double)

  def (c: Circle).circumference: Double = c.radius * math.Pi * 2

  val circle = Circle(0, 0, 1)
  circle.circumference
  assert(circle.circumference == circumference(circle))

  trait StringSeqOps {
    def (xs: Seq[String]).longestStrings = {
      val maxLength = xs.map(_.length).max
      xs.filter(_.length == maxLength)
    }
  }
  given ops1: StringSeqOps

  List("here", "is", "a", "list").longestStrings

  locally {
    object ops2 extends StringSeqOps
    import ops2.longestStrings
    List("here", "is", "a", "list").longestStrings
  }

  def (x: String) < (y: String) = x.compareTo(y) < 0
  def [Elem](x: Elem) #: (xs: Seq[Elem]) = x +: xs

  assert("a" < "bb")
  val xs = 1 #: Vector(2, 3)

  def [T](xs: List[T]) second =
    xs.tail.head

  def [T](xs: List[List[T]]) flattened =
    xs.foldLeft[List[T]](Nil)(_ ++ _)

  def [T: Numeric](x: T) + (y: T): T =
    summon[Numeric[T]].plus(x, y)

  List(1, 2, 3).second[Int]

  given stringOps: (xs: Seq[String]) extended with {
    def longestStrings: Seq[String] = {
      val maxLength = xs.map(_.length).max
      xs.filter(_.length == maxLength)
    }
  }

  given listOps: [T](xs: List[T]) extended with
    def second = xs.tail.head
    def third: T = xs.tail.tail.head


  given [T](xs: List[T])(given Ordering[T]) extended with
    def largest(n: Int) = xs.sorted.takeRight(n)

  given stringOps1: AnyRef {
    def (xs: Seq[String]).longestStrings: Seq[String] = {
      val maxLength = xs.map(_.length).max
      xs.filter(_.length == maxLength)
    }
  }

  given listOps1: AnyRef {
    def [T](xs: List[T]) second = xs.tail.head
    def [T](xs: List[T]) third: T = xs.tail.tail.head
  }

  given AnyRef {
    def [T](xs: List[T]) largest (given Ordering[T])(n: Int) =
      xs.sorted.takeRight(n)
  }

end ExtMethods