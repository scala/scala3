trait Foo {
  type A = (Any { type T = Int })
  type B = (Any { type S = String })
  def a: A
  def b: B
  def aandb: A & B = b // error: found: B, required: A & B
}
