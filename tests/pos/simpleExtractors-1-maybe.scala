//> using options -Yexplicit-nulls
import language.experimental.magic
import scala.magic.*
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
  def unapply(arg: Any): Any? = Ok(arg)
}

object BarSeq {
  def unapplySeq(arg: Any): Seq[Any]? = List(arg)
}

object Baz {
  def unapply[T](arg: T): T? = Ok(arg)
}

object BazSeq {
  def unapplySeq[T](arg: T): Seq[T]? = List(arg)
}
