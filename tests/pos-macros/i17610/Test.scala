// Main.scala
object Main {
  type MyF[A]

  trait ProviderProcessor {
    def apply(simple: Simple): MyF[Int]
  }

  trait Codec {
    def apply[A]: MyF[A]
  }

  trait Simple {
    def a0: Int
  }

  def test(): Unit = {
    val p= Macros()
  }
}
