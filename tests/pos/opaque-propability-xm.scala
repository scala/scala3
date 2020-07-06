object prob {
  opaque type Probability = Double

  implicit object Probability {
    def apply(n: Double): Option[Probability] =
      if (0.0 <= n && n <= 1.0) Some(n) else None

    def unsafe(p: Double): Probability = {
      require(0.0 <= p && p <= 1.0, s"probabilities lie in [0, 1] (got $p)")
      p
    }

    def asDouble(p: Probability): Double = p

    val Never: Probability = 0.0
    val CoinToss: Probability = 0.5
    val Certain: Probability = 1.0

    implicit val ordering: Ordering[Probability] =
      implicitly[Ordering[Double]]

    extension (p1: Probability) def unary_~ : Probability = Certain - p1
    extension (p1: Probability) def & (p2: Probability): Probability = p1 * p2
    extension (p1: Probability) def | (p2: Probability): Probability = p1 + p2 - (p1 * p2)

    extension (p1: Probability) def isImpossible: Boolean = p1 == Never
    extension (p1: Probability) def isCertain: Boolean = p1 == Certain

    import scala.util.Random

    extension (p1: Probability) def sample(r: Random = Random): Boolean = r.nextDouble <= p1
    extension (p1: Probability) def toDouble: Double = p1
  }

  val caughtTrain = Probability.unsafe(0.3)
  val missedTrain = ~caughtTrain
  val caughtCab = Probability.CoinToss
  val arrived = caughtTrain | (missedTrain & caughtCab)

  println((1 to 5).map(_ => arrived.sample()).toList)
}
