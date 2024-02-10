import scala.deriving.Mirror

object C:
  def useR2: Unit =
    val mirror = summon[Mirror.ProductOf[R2]]
    mirror.fromTuple((3, "asd"))
