case class Foo[+X[_]](will: X[Int]) {
  def foo[Y[_]](right: Foo[Y]) = Foo.doFoo(this, right)
}

class A[X] { def crash = true }
class B[X]

object Foo {
  def doFoo[X[_]](left: Foo[X], right: Foo[X]): Foo[X] = right

  def main(args: Array[String]): Unit = {
    val fooA = Foo(new A[Int])
    val fooB = Foo(new B[Int])
    // The type for this is inferred correctly to Foo[A|B]
    val fine = doFoo(fooA, fooB)
    // This throws a ClassCastException because fooB isn't a Foo[A]
    val oops: Foo[A] = fooA.foo(fooB) // error: found: Foo[B], required: Foo[A]
    println(oops.will.crash)
  }
}
