object Test {

  class Encoder { def apply(x: Int): Int = x }
  given as Encoder

  the[Encoder](2)

}