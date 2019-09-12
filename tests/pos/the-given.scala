object Test {

  class Encoder { def apply(x: Int): Int = x }
  given as Encoder

  summon[Encoder](2)

}