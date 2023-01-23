// scalac: -Wunused:explicits

/* This goes around the "trivial method" detection */
val default_val = 1

def f1(a: Int) = a // OK
def f2(a: Int) = default_val // error
def f3(a: Int)(using Int) = a // OK
def f4(a: Int)(using Int) = default_val // error
def f6(a: Int)(using Int) = summon[Int] // error
def f7(a: Int)(using Int) = summon[Int] + a // OK

package scala2main.unused.args:
  object happyBirthday {
    def main(args: Array[String]): Unit = println("Hello World") // error
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
  val b = (i: Int) => default_val // error
  val c = (_: Int) => default_val // OK

package foo.test.trivial:
  /* A twisted test from Scala 2 */
  class C {
    def answer: 42 = 42
    object X
    def g0(x: Int) = ??? // OK
    def f0(x: Int) = () // OK
    def f1(x: Int) = throw new RuntimeException // OK
    def f2(x: Int) = 42 // OK
    def f3(x: Int): Option[Int] = None // OK
    def f4(x: Int) = classOf[Int] // OK
    def f5(x: Int) = answer + 27 // OK
    def f6(x: Int) = X // OK
    def f7(x: Int) = Y // OK
    def f8(x: Int): List[C] = Nil // OK
    def f9(x: Int): List[Int] = List(1,2,3,4) // error
    def foo:Int = 32  // OK
    def f77(x: Int) = foo // error
  }
  object Y
