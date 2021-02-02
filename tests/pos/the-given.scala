object Test {

  class Encoder { def apply(x: Int): Int = x }
  given Encoder with {}

  summon[Encoder](2)

}