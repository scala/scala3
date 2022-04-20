import scala.deriving.Mirror

class Standalone
case object Standalone

case class WithCompanionCaseClass(i: Int)
case object WithCompanionCaseClass

@main def Test: Unit =

  val mStandalone = summon[Mirror.Of[Standalone.type]]
  assert(mStandalone eq Standalone) // the object is its own mirror
  assert(mStandalone.isInstanceOf[Mirror.Singleton]) // object extends Mirror.Singleton because its its own mirror.

  val mWithCompanion = summon[Mirror.Of[WithCompanionCaseClass.type]]
  assert(mWithCompanion ne WithCompanionCaseClass) // the object is not its own mirror
  assert(mWithCompanion.isInstanceOf[Mirror.SingletonProxy]) // its companion is a case class, so the mirror is a proxy
  assert(!mWithCompanion.isInstanceOf[Mirror.Singleton]) // it can not extend Mirror.Singleton because its companion is a case class.
  assert(WithCompanionCaseClass.isInstanceOf[Mirror.Product]) // its companion is a case class, so the mirror is a product.
