class A[T]
// Unpickling the parents of B1 will lead to unpickling a call to B1.<init>
class B1 extends A[C1]
class C1 extends B1

// Unpickling the parents of B2 will lead to unpickling a call to the secondary B2.<init>
class B2 extends A[C2] {
  def this(x: Int) = this()
}
class C2 extends B2(1)

// Unpickling the parents of B3 will lead to unpickling a call to B3#Hi
class B3 extends A[B3#Hi[Int]] {
  type Hi[X] = X
}
