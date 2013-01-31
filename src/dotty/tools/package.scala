package dotty

package object tools {
  type FatalError = scala.reflect.internal.FatalError
  val FatalError = scala.reflect.internal.FatalError

  type MissingRequirementError = scala.reflect.internal.MissingRequirementError
  val MissingRequirementError = scala.reflect.internal.MissingRequirementError

  val ListOfNil = List(Nil)
}
