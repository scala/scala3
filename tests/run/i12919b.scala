import scala.deriving.Mirror


sealed trait WithCompanionSealedTrait
case object WithCompanionSealedTrait:
  case class FirstChild(x: Int) extends WithCompanionSealedTrait

@main def Test: Unit =

  val mWithCompanionSum = summon[Mirror.SumOf[WithCompanionSealedTrait]]
  assert(mWithCompanionSum.ordinal(WithCompanionSealedTrait.FirstChild(1)) == 0)
  assert(mWithCompanionSum eq WithCompanionSealedTrait) // case object caches sum mirror of its companion

  val mWithCompanionSingleton = summon[Mirror.ProductOf[WithCompanionSealedTrait.type]]
  assert(mWithCompanionSingleton.fromProduct(EmptyTuple) == WithCompanionSealedTrait)
  assert(mWithCompanionSingleton.isInstanceOf[Mirror.SingletonProxy])
  assert(mWithCompanionSingleton ne WithCompanionSealedTrait) // proxy mirror is never the object itself
