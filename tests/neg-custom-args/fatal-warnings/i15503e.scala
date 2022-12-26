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