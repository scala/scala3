trait GeneratedEnum
trait Decoder[A]

object Decoder:
  given Decoder[Int] = ???

object GeneratedEnumDecoder:

  given [A <: GeneratedEnum]: Decoder[A] =
    summon[Decoder[Int]]
    ???