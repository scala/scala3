// Compile with -strict -Xfatal-warnings -deprecation
import scala.annotation.infix
class C {
  @infix def op(x: Int): Int = ???
  def meth(x: Int): Int = ???
  def matching(x: Int => Int) = ???
  def +(x: Int): Int = ???
}

val c = C()
def test() = {
  c op 2
  c.meth(2)

  c.op(2)
  c meth 2    // error: should not be used as infix operator
  c `meth` 2  // OK, sincd `meth` is backquoted
  c + 3       // OK, since `+` is symbolic
  1 to 2      // OK, since `to` is defined by Scala-2
  c meth {    // OK, since `meth` is followed by `{...}`
    3
  }
  c matching {    // OK, since `meth` is followed by `{...}`
    case x => x
  }

  @infix class Or[X, Y]
  class AndC[X, Y]
  @infix type And[X, Y] = AndC[X, Y]
  @infix type &&[X, Y] = AndC[X, Y]

  class Map[X, Y]

  val x1: Int Map String = ???     // error
  val x2: Int Or String = ???      // OK since Or is declared `@infix`
  val x3: Int AndC String = ???    // error
  val x4: Int `AndC` String = ???  // OK
  val x5: Int And String = ???     // OK
  val x6: Int && String = ???

  case class Pair[T](x: T, y: T)
  @infix case class Q[T](x: T, y: T)

  object PP {
    @infix def unapply[T](x: Pair[T]): Option[(T, T)] = Some((x.x, x.y))
  }

  val p = Pair(1, 2)
  val Pair(_, _) = p
  val _ Pair _ = p   // error
  val _ `Pair` _ = p // OK
  val (_ PP _): @unchecked = p     // OK

  val q = Q(1, 2)
  val Q(_, _) = q
  val _ Q _ = q   // OK


}
