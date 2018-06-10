class Foo {
  def bar(x: Any): Unit = x match {
    case Bar(a) => println(a)
    case BarSeq(a) => println(a)
    case BarSeq(a, b) => println(a)
  }
  def baz(x: Any): Unit = x match {
    case Baz(a) => println(a)
    case BazSeq(a) => println(a)
    case BazSeq(a, b) => println(a)
  }
}

object Bar {
  def unapply(arg: Any): Option[Any] = Some(arg)
}

object BarSeq {
  def unapplySeq(arg: Any): Option[Seq[Any]] = Some(List(arg))
}

object Baz {
  def unapply[T](arg: T): Option[T] = Some(arg)
}

object BazSeq {
  def unapplySeq[T](arg: T): Option[Seq[T]] = Some(List(arg))
}
