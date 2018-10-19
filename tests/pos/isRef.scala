trait Foo {
  type A = (Any { type T = Int })
  type B = (Any { type S = String })
  def b: B
  def aorb: A | B = b
}
trait Foo2 {
  type A[_]
  type B[_]
  def b: B[Int]
  def aorb: A[Int] | B[Int] = b
}
