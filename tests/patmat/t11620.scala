sealed trait A[+T]
case class A1[+T](t : T       ) extends A[T]
case class A2[+T](t1: T, t2: T) extends A[T]

sealed trait B[+T] {
  type AA[+U] <: A[U]
  def a: AA[T]
}
object B {
  type Aux[+_A[+_], +T] = B[T] { type AA[+U] <: _A[U] }
  object Aux {
    def unapply[_A[+U] <: A[U], T](b: Aux[_A, T]): Some[_A[T]] = Some(b.a)
  }

  def apply[_A[+U] <: A[U], T](_a: _A[T]): Aux[_A, T] =
    new B[T] { type AA[+U] = _A[U] ; val a: _A[T] = _a }

  def unapply[T](b: B[T]): Some[b.AA[T]] = Some(b.a)
}

def foo[T](b: B[T]) = b match {
  case B(A1(t)) => t
  case B(A2(t, _)) => t
}

def foo2[_A[+U] <: A[U], T](b: B.Aux[_A, T]) = b match {
  case B.Aux(a @ A1(_   )) => a.t
  case B.Aux(a @ A2(_, _)) => a.t1  // 👎 (false-positive): unreachable code
}

def foo3[_A[+U] <: A[U], T](b: B.Aux[_A, T]) = b match {
  case B.Aux(a: A1[T]) => a.t
  case B.Aux(a: A2[T]) => a.t1  // 👎 (false-positive): unreachable code
}

def foo4[T](b: B[T]) = b match {
  case B(A1(t)) => t  // 👎 (false-negative): incomplete match
}
