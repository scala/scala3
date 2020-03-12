trait Foo[A]; trait Bar

object Test {
  def qux1[A](implicit A: Foo[A], B: Bar = new Bar {}): Bar = B
  def qux2[A: Foo](implicit B: Bar = new Bar {}): Bar = B
}