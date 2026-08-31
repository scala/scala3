//> using options -Yexplicit-nulls
import language.experimental.errorHandling
case class Foo(a: String, b: String)

object Bar {
  def unapply(s: String): Long? =
    try { s.toLong } catch { case _ => null }
}

object Test {
  def main(args: Array[String]): Unit = {
    val f = Foo("1", "2")
    f match {
      case Foo(Bar(1), Bar(2)) => 1
      case Foo(Bar(i), Bar(j)) if i >= 0 => 2
      case _ => 3
    }
  }
}

