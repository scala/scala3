object Test {
  trait Marker

  sealed trait T
  trait A extends T with Marker
  trait B extends T with Marker
  case object C extends T

  def m1(s: T & Marker) = s match {
    case _: A => ;
  }

  def m2(s: Marker & T) = s match {
    case _: A => ;
  }

  def m3(s: (A | B) & Marker) = s match {
    case _: A => ;
  }
}
