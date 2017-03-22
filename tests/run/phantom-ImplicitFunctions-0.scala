
object Boo extends Phantom {
  type Slimer <: this.Any
  implicit def phantom: Slimer = assume[Slimer]
}


object Test {

  def main(args: Array[String]): Unit = {
    implicit val world: String = "world!"
    import Boo._

    val i1 = (implicit (s: Slimer) => true)
    val i2 = {implicit (s: Slimer) => false}

    assert(i1)
    assert(!i2)

    val x: implicit (String, Slimer) => Boolean = { implicit (s: String, p: Slimer) => s.length > 2 }

    val xx: implicit (String, Int, Slimer) => Int = implicit (x: String, y: Int, p: Slimer) => x.length + y

    val y: (String, Slimer) => Boolean = x

    object nested {
      implicit val empty: String = ""
      assert(!x)
    }

    val yy: (String, Int, Slimer) => Any = xx

    val z1: implicit (String, Slimer) => Boolean = implicitly[String].length >= 2
    assert(z1)

    type StringlyBool = implicit (String, Slimer) => Boolean

    val z2: StringlyBool = implicitly[String].length >= 2
    assert(z2)

    type Stringly[T] = implicit (String, Slimer) => T

    val z3: Stringly[Boolean] = implicitly[String].length >= 2
    assert(z3)

    type GenericImplicit[X, Y <: Slimer] = implicit (X, Y) => Boolean

    val z4: GenericImplicit[String, Slimer] = implicitly[String].length >= 2
    assert(z4)

    val b = x("hello", phantom)

    val b1: Boolean = b

    val bi = x

    val bi1: Boolean = bi

    val c = xx("hh", 22, phantom)

    val c1: Int = c

    def foo(s: String): Stringly[Int] = 42

    (if ("".isEmpty) foo("") else foo("")).apply("", phantom)
  }
}
