object Test {

  sealed abstract class A

  case object A1 extends A

  case object A2 extends A

  sealed abstract class B

  case object B1 extends B

  case object B2 extends B

  sealed abstract class C

  final case class One(a: A, b: B) extends C

  final case class Two(b: B, a: A) extends C

  def foo(c: C): Unit = c match {
    case One(A1, B1) =>
    case One(A2, B1) =>
    case One(A1, B2) =>
    case One(A2, B2) =>
    case Two(B1, A1) =>
    case Two(B2, A1) =>
  }
}