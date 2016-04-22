class X

class Message[-A]
class Seg[+A]
class IChan[A] {
  def add[B >: A](x: Seg[B])(implicit ev: Message[B]): IChan[B] = ???
}

class Test {
  def test: Unit = {
    implicit val mx: Message[X] = ???
    val fx: IChan[X] = ???
    val sx: Seg[X] = ???
    // the implicit `mx` should be used even though the type parameter of Message is contravariant
    fx.add(sx)
  }
}
