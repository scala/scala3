object test {

  trait X { def foo(x: Int): Int; def bar = foo(2) }

  val x: X = (x: Int) => 2  // should be a closure

  trait T {
    var f = 2
    def foo(x: Int): Int
  }

  val t: T = (x: Int) => 2   // needs to be an anonymous class because of defined field

  trait U extends T

  val u: U = (x: Int) => 2   // needs to be an anonymous class because of inherited field

  trait V extends Exception { def foo(x: Int): Int }

  val v: V = (x: Int) => 2   // needs to be an anonymous class because the trait extends a non-object class

  trait Y extends X {
    def baz = super.bar
  }

  val y: Y = (x: Int) => 2   // needs to be an anonymous class because of super accessor

  trait Z {
    def foo(x: Int): Int; println("hi there!")
  }
  trait ZZ extends Z

  val z: Z = (x: Int) => 2   // needs to be an anonymous class because trait has initialization code
  val zz: ZZ = (x: Int) => 2   // needs to be an anonymous class becaiuse trait has initialization code

  abstract class C {
    def foo(x: Int): Int

    trait I { def foo(x: Int): Int }

  }

  val c: C = (x: Int) => 2   // needs to be an anonymous class because C is not a trait

  val ci: c.I = (x: Int) => 2 // needs to be an anonymous class because it needs an outer pointer


  val pf: PartialFunction[Int, Int] = {
    case 1 => 1
    case 2 => 2
  }

  val qf: PartialFunction[(Int, String), Int] = {
    case (1, "abc") => 1
    case _ => 2
  }

  val rf: PartialFunction[(Int, AnyRef), Int] = {
    case (_: Int, _: String) => 1
    case _ => 2
  }

  val sf: PartialFunction[Any, Int] = {
    case x: String if x == "abc" => 1
  }
}

// From: neg/sammy_poly
// synthesizeSAMFunction where the sam type is not fully defined
class T {
  trait F[T, U] { def apply(x: T): U }
  // this is an inner trait, that will receive an abstract $outer pointer. Not a SAM.
  def app[T, U](x: T)(f: F[T, U]): U = f(x)
  app(1)(x => List(x))
}

object SI9943 {

class Foo[T] {
  def toMap[K, V](implicit ev: Foo[T] <:< Foo[(K, V)]): Foo[Map[K, V]] = null
  def toMap[K](keySelector: T => K): Foo[Map[K, T]] = null
}

object Foo {
  val f: Foo[Int] = null
  val m = f.toMap(_ % 2)
}
}
