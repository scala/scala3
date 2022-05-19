import scala.deriving.Mirror

case class Standalone(i: Int)
object Standalone

case class WithCompanionCaseClass(i: Int)
case object WithCompanionCaseClass

@main def Test: Unit =

  val mStandalone = summon[Mirror.ProductOf[Standalone]]
  assert(mStandalone eq Standalone) // the companion object is the mirror for the case class

  val mWithCompanion = summon[Mirror.ProductOf[WithCompanionCaseClass]]
  assert(mWithCompanion ne WithCompanionCaseClass) // A case object can not be the mirror of a companion case class.

  val mWithCompanionCaseObject = summon[Mirror.ProductOf[WithCompanionCaseClass.type]]
  assert(mWithCompanionCaseObject eq WithCompanionCaseClass) // A case object is its own mirror.
