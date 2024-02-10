import scala.deriving.Mirror

object C:
  def useR2: Unit =
    summon[Mirror.Of[R2]]
