package unsafeNulls

class Unsafe_1 {

  def this(s: String) = {
    this()
    ???
  }

  def foo(s: String): String = {
    if (s == null) then "nullString"
    else s
  }
  def bar[T >: String](s: T): T = {
    ???
  }
  def bar2[T >: String | Null](s: T): T = {
    ???
  }
  def bar3[T <: Function1[String,String]](g: T): T = g
  def bar4[HK[_]](i: String): HK[String] = ???

  var member: String = ???
}

trait A extends C {
  var stringA: String
}

trait B extends C {
  var stringB: String
}

trait C {
  var stringC: String
}


object Foo {
  extension (c: C)
    def reverse: String = c.stringC.reverse

  val bar: String = null
  def id[T](t: T): T = t
  var refinement: Unsafe_1 { var b: String }  = new Unsafe_1 { var b: String = "???" }
  var singleton: bar.type = bar
  var intersection: A & B = ???
  var union: A | B = ???
}

class F(var x: String, var y: G)

class G(var z: Int)

object F {
  def unapply(f: F): (String, G) = (f.x, f.y)
  var many: (List[F] | String | List[Int]) = null
}

class H {
  val s: String = null
}

class I[+A](val value: A)

class J(val a: String) {}

class L[-A](f: A => Unit)

class M {}

object M {
  def test(input: => String): String = "foo " + input
}


class N[F[_]] {
  def accept[A](arg: F[A]): Nothing = ???
}

class S[X]
object S {  def show[X] = "dummyStr" }
class T
class U[Y](a: Y)
def expects(take: (T) ?=> U[String]) = ???

// i23935
object ZIO:
  def attempt[A](code: => A): ZIO[Any, Throwable, A] = ???

trait ZIO[-R, +E, +A]:
  final def flatten[R1 <: R, E1 >: E, B](using A IsSubtypeOfOutput ZIO[R1, E1, B]): ZIO[R1, E1, B] = ???

infix sealed abstract class IsSubtypeOfOutput[-A, +B]
object IsSubtypeOfOutput:
  given [A, B](using A <:< B): IsSubtypeOfOutput[A, B] = ???
