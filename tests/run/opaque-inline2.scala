
import compiletime.*

object refined:
  opaque type Positive = Int

  object Positive extends PositiveFactory

  trait PositiveFactory:
    inline def apply(inline value: Int): Positive =
      inline if value < 0 then error(codeOf(value) + " is not positive.")
      else value

    transparent inline def safe(value: Int): Positive | IllegalArgumentException =
      if value < 0 then IllegalArgumentException(s"$value is not positive")
      else value: Positive

@main def Test: Unit =
  import refined.*
  val eight = Positive(8)
  // val negative = Positive(-1) // This correctly produces a compile error "-1 is not positive."
  // val random = Positive(scala.util.Random.nextInt()) // This correctly produces a compile error about being unable to inline the method call
  val random = Positive.safe(scala.util.Random.nextInt())
  val safeNegative = Positive.safe(-1)
  val safeFive = Positive.safe(5)
  println(eight)
  println(random)
  println(safeFive)