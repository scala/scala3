import scala.annotation._
class Immutable extends Annotation with SubTypeAnnotation

object Test extends App {

  def assertImmutable[T](n: T): T @ Immutable = ???

  def g[S](x: S, y: S) = if (???) x else y

  def f(n: Int @ Immutable): Int @ Immutable =
    if n == 0 then assertImmutable(0)
    else {
      val n1 = f(f(n))
      val x = g(n1, n1)
      val y = g(n1, assertImmutable(3))
      val z = g(x, y)
      val r =
        if (???) x
        else if (???) y
        else if (???) z
        else assertImmutable(n1 - 1)
      val s = g(assertImmutable(List(1, 2, 3)), assertImmutable("a"))
      val scheck: java.io.Serializable = s
      r
    }
}