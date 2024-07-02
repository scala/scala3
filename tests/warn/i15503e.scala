//> using options  -Wunused:explicits

object Foo {
  /* This goes around the "trivial method" detection */
  val default_val = 1

  private def f1(a: Int) = a // OK
  private def f2(a: Int) = default_val // warn
  private def f3(a: Int)(using Int) = a // OK
  private def f4(a: Int)(using Int) = default_val // warn
  private def f6(a: Int)(using Int) = summon[Int] // warn
  private def f7(a: Int)(using Int) = summon[Int] + a // OK
}

package scala2main.unused.args:
  object happyBirthday {
    def main(args: Array[String]): Unit = println("Hello World") // ok
  }

package scala2main:
  object happyBirthday {
    def main(args: Array[String]): Unit = // OK
      println(s"Hello World, there are ${args.size} arguments")
  }

package scala3main:
  /* This goes around the "trivial method" detection */
  val default_unit = ()
  @main def hello = println("Hello World") // OK

package foo.test.lambda.param:
  val default_val = 1
  val a = (i: Int) => i // OK
  val b = (i: Int) => default_val // warn
  val c = (_: Int) => default_val // OK

package foo.test.trivial:
  /* A twisted test from Scala 2 */
  class C {
    def answer: 42 = 42
    object X
    private def g0(x: Int) = ??? // OK
    private def f0(x: Int) = () // OK
    private def f1(x: Int) = throw new RuntimeException // OK
    private def f2(x: Int) = 42 // OK
    private def f3(x: Int): Option[Int] = None // OK
    private def f4(x: Int) = classOf[Int] // OK
    private def f5(x: Int) = answer + 27 // OK
    private def f6(x: Int) = X // OK
    private def f7(x: Int) = Y // OK
    private def f8(x: Int): List[C] = Nil // OK
    private def f9(x: Int): List[Int] = List(1,2,3,4) // warn
    private def foo:Int = 32  // OK
    private def f77(x: Int) = foo // warn
  }
  object Y

package foo.test.i16955:
  class S(var r: String) // OK

package foo.test.i16865:
  trait Foo:
    def fn(a: Int, b: Int): Int // OK
  trait Bar extends Foo

  object Ex extends Bar:
    def fn(a: Int, b: Int): Int = b + 3 // OK

  object Ex2 extends Bar:
    override def fn(a: Int, b: Int): Int = b + 3 // OK