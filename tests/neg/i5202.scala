object Test {
  val f: (Int => Int) | (String => Int) = (a: Int) => a + 3

  f.apply(5) // error - found: Int expected: Int & String
  f("c")     // error - found: String expected: Int & String
}

class Foo[A] {
  def foo(a: A): Unit = {}
}
class Co[+A] {
  def foo(a: A): Unit = {} // error: contravariant occurs in covariant position
  def bar: A = ???
}
class Contra[-A] {
  def foo(a: A): Unit = {}
}

object Test2 {
  def main(args: Array[String]): Unit = {
    val x: Foo[Int] | Foo[String] = new Foo[Int]
    x.foo("") // error, found: String, required: Int & String
    val y: Contra[Int] | Contra[String] = new Contra[Int]
    y.foo("") // error, found: String, required: Int & String
    val z: Co[Int] | Co[String] = new Co[Int]
    z.foo("") // OK
    val s: String = z.bar // error: found Int | String, required: String
  }
}