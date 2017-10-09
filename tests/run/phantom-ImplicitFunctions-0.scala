
object Boo extends Phantom {
  type Slimer <: this.Any
  implicit def phantom: Slimer = assume
}


object Test {

  def main(args: Array[String]): Unit = {
    implicit val world: String = "world!"
    import Boo._

    val i1 = (implicit (s: Slimer) => true)
    val i2 = {implicit (s: Slimer) => false}

    assert(i1)
    assert(!i2)

    val x: implicit (Slimer, Slimer) => Boolean = { implicit (s: Slimer, p: Slimer) => false }

    val xx: implicit (Slimer, Slimer, Slimer) => Int = implicit (x: Slimer, y: Slimer, p: Slimer) => 5

    val y: (Slimer, Slimer) => Boolean = implicit (x: Slimer, y: Slimer) => true

    object nested {
      implicit val empty: String = ""
      assert(!x)
    }

    val yy: (Slimer, Slimer, Slimer) => Any = xx

    val z1: implicit (Slimer, Slimer) => Boolean = true
    assert(z1)

    type SlimyBool = implicit Slimer => Boolean

    // TODO: should support phantom implicitly?
    //  val z2: SlimyBool = {
    //    implicitly[Slimer]
    //    true
    //  }
    //  assert(z2)

    type GenericImplicit[X <: Slimer, Y <: Slimer] = implicit (X, Y) => Boolean

    val z4: GenericImplicit[Slimer, Slimer] = true
    assert(z4)

    val b = x(phantom, phantom)

    val b1: Boolean = b

    val bi = x

    val bi1: Boolean = bi

    val c = xx(phantom, phantom, phantom)

    val c1: Int = c
  }
}
