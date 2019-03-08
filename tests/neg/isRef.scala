trait Foo {
  type A = (Any { type T = Int })
  type B = (Any { type S = String })
  def a: A
  def b: B
  def aandb: A & B = b // error: found: B, required: A & B
}
trait Foo2 {
  type A[_]
  type B[_]
  def b: B[Int]
  def aandb: A[Int] & B[Int] = b // error: found: B[Int], required: A[Int] & B[Int]
}
