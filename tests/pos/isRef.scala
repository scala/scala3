trait Foo {
  type A = (Any { type T = Int })
  type B = (Any { type S = String })
  def b: B
  def aorb: A | B = b
}
