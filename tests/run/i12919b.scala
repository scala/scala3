import scala.deriving.Mirror


sealed trait WithCompanionSealedTrait
case object WithCompanionSealedTrait:
  case class FirstChild(x: Int) extends WithCompanionSealedTrait

@main def Test: Unit =

  val mWithCompanionSum = summon[Mirror.SumOf[WithCompanionSealedTrait]]
  assert(mWithCompanionSum.ordinal(WithCompanionSealedTrait.FirstChild(1)) == 0)
  assert(mWithCompanionSum ne WithCompanionSealedTrait) // A case object can not be the mirror of a companion case class.

  val mWithCompanionSingleton = summon[Mirror.ProductOf[WithCompanionSealedTrait.type]]
  assert(mWithCompanionSingleton.fromProduct(EmptyTuple) == WithCompanionSealedTrait)
  assert(mWithCompanionSingleton.isInstanceOf[Mirror.Singleton]) // case object is its own mirror.
  assert(mWithCompanionSingleton eq WithCompanionSealedTrait) // case object is its own mirror.
