object Test {

  class Encoder { def apply(x: Int): Int = x }
  implied for Encoder

  the[Encoder](2)

}