// Regression test: wildcardArgOK must handle mixed wildcard/concrete type args
// where both arg and formal have TypeBounds in the same position

trait Format
trait Codec[L, H, F <: Format]  // invariant

trait Variant[O] {
  def codec: Codec[?, O, ? <: Format]
}

object Test {
  def decode[L, H](codec: Codec[L, H, ? <: Format]): Int = 1
  def decode[L, H](other: String): Int = 2

  def test[O](v: Variant[O]): Int = decode(v.codec)

  def main(args: Array[String]): Unit = {
    val variant = new Variant[String] {
      def codec: Codec[?, String, ? <: Format] = null
    }
    assert(test(variant) == 1)
  }
}
