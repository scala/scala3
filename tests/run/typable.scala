import scala.reflect._

object Test with
  def main(args: Array[String]): Unit =
    assert(f[String])
    assert(!f[Int])

  def f[T: Typeable]: Boolean =
    "abc" match
      case x: T => true
      case _ => false
