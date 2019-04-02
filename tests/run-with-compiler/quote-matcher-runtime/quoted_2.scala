
import Macros._

import scala.runtime.quoted.Matcher._

object Test {

  def main(args: Array[String]): Unit = {
    val b: Boolean = true
    val x: Int = 42
    val y: Int = 52
    var z: Int = 62
    var z2: Int = 62
    def f(a: Int): Int = 72
    def f2(a: Int, b: Int): Int = 72
    def g[A]: A = ???
    def h[A](a: A): A = a
    def fs(a: Int*): Int = 72

    // Matches
    println("Matches")
    println()

    matches(1, 1)
    matches(1: Int, 1)
    matches(1: Int, 1: Int)
    matches(1, 1: Int)
    matches(3, hole[Int])
    matches(x, hole[Int])
    matches(5, hole[Any])
    matches(6 + x, hole[Int])
    matches(6 + x, 6 + hole[Int])
    matches(6 + x, hole[Int] + x)
    matches(6 + x, hole[Int] + hole[Int])
    matches(6 + x + y, 6 + hole[Int] + y)
    matches(f(4), hole[Int])
    matches(f(5), f(hole[Int]))
    matches(g[Int], hole[Int])
    matches(h[Int](7), hole[Int])
    matches(h[Int](8), h[Int](hole[Int]))
    matches(this, this)
    matches(this, hole[this.type])
    matches(new Foo(1), new Foo(1))
    matches(new Foo(1), hole[Foo])
    matches(new Foo(1), new Foo(hole[Int]))
    matches(if (b) x else y, if (b) x else y)
    matches(if (b) x else y, hole[Int])
    matches(if (b) x else y, if (hole[Boolean]) hole[Int] else hole[Int])
    matches(while (b) x, while (b) x)
    matches(while (b) x, hole[Unit])
    matches(while (b) x, while (hole[Boolean]) hole[Int])
    matches({z = 4}, {z = 4})
    matches({z = 4}, hole[Unit])
    matches({z = 4}, {z = hole[Int]})
    matches({z = 4}, {varHole = 4})
    matches(1, {1})
    matches({1}, 1)
    // Should these match?
    // matches({(); 1}, 1)
    // matches(1, {(); 1})
    matches(fs(), fs())
    matches(fs(), fs(hole[Seq[Int]]: _*))
    matches(fs(1, 2, 3), fs(1, 2, 3))
    matches(fs(1, 2, 3), fs(hole[Int], hole[Int], 3))
    matches(fs(1, 2, 3), fs(hole[Seq[Int]]: _*))
    matches(f2(1, 2), f2(1, 2))
    matches(f2(a = 1, b = 2), f2(a = 1, b = 2))
    matches(f2(a = 1, b = 2), f2(a = hole[Int], b = hole[Int]))
    // Should these match?
    // matches(f2(a = 1, b = 2), f2(1, 2))
    // matches(f2(b = 2, a = 1), f2(1, 2))
    matches(super.toString, super.toString)
    matches(() => "abc", hole[() => String])
    matches((() => "abc")(), (hole[() => String]).apply())
    matches((x: Int) => "abc", hole[Int=> String])
    matches(((x: Int) => "abc")(4), (hole[Int => String]).apply(4))
    matches((x: Int) => "abc", (x: bindHole[Int]) => hole[String])
    matches(StringContext("abc", "xyz"), StringContext("abc", "xyz"))
    matches(StringContext("abc", "xyz"), StringContext(hole, hole))
    matches(StringContext("abc", "xyz"), StringContext(hole[Seq[String]]: _*))
    matches({ val a: Int = 45 }, { val a: Int = 45 })
    matches({ val a: Int = 45 }, { val a: bindHole[Int] = hole })
    matches({ val a: Int = 45 }, { lazy val a: Int = 45 })
    matches({ val a: Int = 45 }, { var a: Int = 45 })
    matches({ val a: Int = 45 }, { var a: bindHole[Int] = hole })
    matches({ lazy val a: Int = 45 }, { val a: Int = 45 })
    matches({ lazy val a: Int = 45 }, { lazy val a: Int = 45 })
    matches({ lazy val a: Int = 45 }, { var a: Int = 45 })
    matches({ lazy val a: Int = 45 }, { val a: bindHole[Int] = hole })
    matches({ lazy val a: Int = 45 }, { var a: bindHole[Int] = hole })
    matches({ var a: Int = 45 }, { val a: Int = 45 })
    matches({ var a: Int = 45 }, { lazy val a: Int = 45 })
    matches({ var a: Int = 45 }, { var a: Int = 45 })
    matches({ var a: Int = 45 }, { val a: bindHole[Int] = hole })
    matches({ var a: Int = 45 }, { lazy val a: bindHole[Int] = hole })
    matches({ println(); println() }, { println(); println() })
    matches({ { println() }; println() }, { println(); println() })
    matches({ println(); { println() } }, { println(); println() })
    matches({ println(); println() }, { println(); { println() } })
    matches({ println(); println() }, { { println() }; println() })
    matches({ def a: Int = 45 }, { def a: Int = 45 })
    matches({ def a: Int = 45 }, { def a: bindHole[Int] = hole[Int] })
    matches({ def a(x: Int): Int = 45 }, { def a(x: Int): Int = 45 })
    matches({ def a(x: Int): Int = 45 }, { def a(x: Int, y: Int): Int = 45 })
    matches({ def a(x: Int): Int = 45 }, { def a(x: Int)(y: Int): Int = 45 })
    matches({ def a(x: Int, y: Int): Int = 45 }, { def a(x: Int): Int = 45 })
    matches({ def a(x: Int)(y: Int): Int = 45 }, { def a(x: Int): Int = 45 })
    matches({ def a(x: String): Int = 45 }, { def a(x: String): Int = 45 })
    matches({ def a(x: Int): Int = 45 }, { def a(x: bindHole[Int]): Int = 45 })
    matches({ def a(x: Int): Int = 45 }, { def a(x: bindHole[Int]): bindHole[Int] = 45 })
    matches({ def a(x: Int): Int = x }, { def b(y: Int): Int = y })
    matches({ def a: Int = a }, { def b: Int = b })
    matches({ lazy val a: Int = a }, { lazy val b: Int = b })
    matches(1 match { case _ => 2 }, 1 match { case _ => 2 })
    matches(1 match { case _ => 2 }, hole[Int] match { case _ => hole[Int] })
    matches(??? match { case None => 2 }, ??? match { case None => 2 })
    matches(??? match { case Some(1) => 2 }, ??? match { case Some(1) => 2 })
    matches(??? match { case Some(1) => 2 }, ??? match { case Some(patternHole()) => 2 })
    matches(??? match { case Some(n) => 2 }, ??? match { case Some(patternBindHole(n)) => 2 })
    matches(??? match { case Some(n @ Some(m)) => 2 }, ??? match { case Some(patternBindHole(n @ Some(patternBindHole(m)))) => 2 })
    matches(try 1 catch { case _ => 2 }, try 1 catch { case _ => 2 })
    matches(try 1 finally 2, try 1 finally 2)
    matches(try 1 catch { case _ => 2 }, try hole[Int] catch { case _ => hole[Int] })
    matches(try 1 finally 2, try hole[Int] finally hole[Int])

    // No match
    println()
    println("No match")
    println()

    matches(1, 2)
    matches(4, hole[String])
    matches(6 + x, 7 + hole[Int])
    matches(6 + x, hole[Int] + 4)
    matches(g[Int], hole[String])
    matches(h[Int](7), h[String](hole[String]))
    matches(h[Int](6), h[Int](7))
    matches({z = 4}, {z = 5})
    matches({z = 4}, {z2 = 4})

  }
}

class Foo(a: Int)
