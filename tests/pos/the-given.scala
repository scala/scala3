object Test {

  class Encoder { def apply(x: Int): Int = x }
  delegate for Encoder

  the[Encoder](2)

}