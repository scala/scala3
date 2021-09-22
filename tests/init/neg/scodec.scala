trait Codec[A] { self =>
  final def withContext(context: String): Codec[A] =
    class X extends Codec[A] {
      def decode(bits: String) = 10
      override def toString = s"$self"
    }
    new X

  def decode(bits: String): Int

  def decodeOnly[AA >: A]: Codec[AA] = {
    val sup = this.decodeOnly[AA]
    class Y extends Codec[AA] {
      def decode(bits: String) = sup.decode(bits)
    }
    new Y
  }

}

object codecs {
  class Z extends Codec[String] {
    override def decode(bits: String): Int = 0
  }
  val codec = new Z

  println(codec)   // error

  val n = 10 // prevent early promotion
}
