
object refined:
  opaque type Positive = Int

  object Positive extends PositiveFactory

  trait PositiveFactory:
    inline def apply(value: Int): Positive = value

    def f(x: Positive): Positive = x
    inline def fapply(value: Int): Positive =
      val vv = (value, value) // error: implementation restriction
      f(vv._1)

@main def run: Unit =
  import refined.*
  val x = 9
  val nine = Positive.apply(x)
  val nine1 = Positive.fapply(x)

