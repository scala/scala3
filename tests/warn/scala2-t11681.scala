//> using options -Wunused:params


import Answers._

trait InterFace {
  /** Call something. */
  def call(a: Int, b: String, c: Double): Int
}

trait BadAPI extends InterFace {
  private def f(a: Int,
        b: String,               // warn
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
                    b: String, // no warn (override)
                    c: Double): Int = {
    println(c)
    a
  }

  def meth(x: Int) = x

  override def equals(other: Any): Boolean = true  // OK

  def i(implicit s: String) = answer // warn now

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
}

class Unusing(u: Int) {       // warn
  def f = ???
}

class Valuing(val u: Int)        // OK

class Revaluing(u: Int) { def f = u } // OK

case class CaseyKasem(k: Int)        // OK

case class CaseyAtTheBat(k: Int)(s: String)        // warn unused s

trait Ignorance {
  def f(readResolve: Int) = answer  // warn now
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
  def f(implicit dummy: DummyImplicit) = answer // ok
  def g(dummy: DummyImplicit) = answer // warn now
}
trait Proofs {
  def f[A, B](implicit ev: A =:= B) = answer // ok
  def g[A, B](implicit ev: A <:< B) = answer // ok
  def f2[A, B](ev: A =:= B) = answer // warn now
  def g2[A, B](ev: A <:< B) = answer // warn now
}

trait Anonymous {
  def f = (i: Int) => answer      // warn now

  def f1 = (_: Int) => answer     // OK

  def f2: Int => Int = _ + 1  // OK

  def g = for (i <- List(1)) yield answer    // no warn (that is a patvar)
}
trait Context[A]
trait Implicits {
  def f[A](implicit ctx: Context[A]) = answer // no warn after all; previously, warn implicit param even though marker
  def g[A: Context] = answer // no warn bound that is marker only
}
class Bound[A: Context] // no warn bound that is marker only
object Answers {
  def answer: Int = 42
}

val a$1 = 2
