object Test {
  sealed trait Marker
  trait M1 extends Marker
  sealed trait Marker2 extends Marker
  sealed trait M2 extends Marker2

  sealed trait T
  trait A extends T with M2
  sealed trait T2 extends T
  case class B() extends T2 with Marker
  class C extends T2 with M1
  case object D extends T2 with Marker

  trait Unrelated

  def m1(s: T & Marker) = s match {
    case _: Unrelated => ;
  }
}
