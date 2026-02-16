object ConversionChain {

  class X(val value: Int)

  class Y(val x: X)

  class Z(val y: Y)

  trait Conv[A, B] extends Conversion[A, B]

  given xy: Conv[X, Y] = { (x: X) => new Y(x) }

  given yz: Conv[Y, Z] = { (y: Y) => new Z(y) }

  object ConvUtils {
    implicit def hypotheticalSyllogism[A, B, C]( // implicit def instead of given
        using
        ab: Conv[A, B],
        bc: Conv[B, C]
    ): Conv[A, C] = {

      new Conv[A, C] {
        def apply(a: A): C = bc(ab(a))
      }
    }
  }
  import ConvUtils.hypotheticalSyllogism

  def test(): Unit = {
    val x = new X(42)
    val z: Z = x
  }
}
