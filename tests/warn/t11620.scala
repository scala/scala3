sealed trait A[+T0]
case class A1[+T1](t1: T1) extends A[T1]
case class A2[+T2](t2: T2) extends A[T2]
sealed trait B[+T3] { type AA[+U] <: A[U] ; def a: AA[T3] }
object B { def unapply[T4](b: B[T4]): Some[b.AA[T4]] = Some(b.a) }
class Test:
  def m1[X](b: B[X]): X = b match
    case B(A1(v1)) => v1
    case B(A2(v2)) => v2
