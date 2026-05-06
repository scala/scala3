trait Codec[L, H, F]  // invariant

trait Variant {
  def codec: Codec[?, String, ? <: AnyRef]
}

object Test {
  def decode[L](codec: Codec[L, ? >: String, ? <: AnyRef]): Int = 1
  def decode(other: Int): Int = 2

  def test(v: Variant): Int = decode(v.codec)

  def main(args: Array[String]): Unit = {
    val variant = new Variant {
      def codec: Codec[?, String, ? <: AnyRef] = null
    }
    assert(test(variant) == 1)
  }
}
