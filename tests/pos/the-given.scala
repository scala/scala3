object Test {

  class Encoder { def apply(x: Int): Int = x }
  given Encoder()

  summon[Encoder](2)

}