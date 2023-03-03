// scalac: -Wunused:params
//

import Answers._

trait InterFace {
  /** Call something. */
  def call(a: Int, b: String, c: Double): Int
}

trait BadAPI extends InterFace {
  def f(a: Int,
        b: String,               // error
        c: Double): Int = {
    println(c)
    a
  }
  @deprecated("no warn in deprecated API", since="yesterday")
  def g(a: Int,
        b: String,               // OK
        c: Double): Int = {
    println(c)
    a
  }
  override def call(a: Int,
                    b: String,               // OK
                    c: Double): Int = {
    println(c)
    a
  }

  def meth(x: Int) = x

  override def equals(other: Any): Boolean = true  // OK

  def i(implicit s: String) = answer           // error

  /*
  def future(x: Int): Int = {
    val y = 42
    val x = y               // maybe option to warn only if shadowed
    x
  }
  */
}

// mustn't alter warnings in super
trait PoorClient extends BadAPI {
  override def meth(x: Int) = ???       // OK
  override def f(a: Int, b: String, c: Double): Int = a + b.toInt + c.toInt
}

class Unusing(u: Int) {       // error
  def f = ???
}

class Valuing(val u: Int)        // OK

class Revaluing(u: Int) { def f = u } // OK

case class CaseyKasem(k: Int)        // OK

case class CaseyAtTheBat(k: Int)(s: String)        // error

trait Ignorance {
  def f(readResolve: Int) = answer           // error
}

class Reusing(u: Int) extends Unusing(u)   // OK

// TODO: check
// class Main {
//   def main(args: Array[String]): Unit = println("hello, args")  // OK
// }

trait Unimplementation {
  def f(u: Int): Int = ???        // OK
}

trait DumbStuff {
  def f(implicit dummy: DummyImplicit) = answer // todo // error
  def g(dummy: DummyImplicit) = answer // error
}
trait Proofs {
  def f[A, B](implicit ev: A =:= B) = answer // todo // error
  def g[A, B](implicit ev: A <:< B) = answer // todo // error
  def f2[A, B](ev: A =:= B) = answer // error
  def g2[A, B](ev: A <:< B) = answer // error
}

trait Anonymous {
  def f = (i: Int) => answer      // error

  def f1 = (_: Int) => answer     // OK

  def f2: Int => Int = _ + 1  // OK

  def g = for (i <- List(1)) yield answer    // error
}
trait Context[A]
trait Implicits {
  def f[A](implicit ctx: Context[A]) = answer // error
  def g[A: Context] = answer // error
}
class Bound[A: Context] // error
object Answers {
  def answer: Int = 42
}

val a$1 = 2