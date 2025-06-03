
  // Trait of all extractors with unapply methods
  trait Matcher[A, B]:
    def unapply(x: A): Option[B]

  // An extractor defined by an unappy method
  object Even extends Matcher[Int, Int]:
    def unapply(x: Int): Option[Int] =
      if x % 2 == 0 then Some(x) else None

  // Method using a given extractor in pattern position
  def collect[A, B](xs: List[A], m: Matcher[A, B]): List[B] =
    xs match
      case Nil  => Nil
      case m(x) :: xs1 => x :: collect(xs1, m)
      case _ :: xs1 => collect(xs1, m)

  @main def test =
    val xs = List(1, 2, 3, 4)
    val ys = collect(xs, Even)
    println(ys)


