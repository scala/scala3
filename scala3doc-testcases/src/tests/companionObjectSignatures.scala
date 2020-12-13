package tests
package companionObjectSignatures

case class CaseClass(parameterOfClassConstructor: String)
{
  val classProperty1: String
    = ???
  val classProperty2: String
    = ???
  def methodInClass1(): String
    = ???

  def methodInClass2(): CaseClass
    = ???
}

case object CaseClass
{
  val parameterOfObject: String
    = "asd"

  def methodInCompanion1(): String
    = ???

  def methodInCompanion2(): CaseClass
    = ???
}

case class WithoutExplicitCompanion(parameter: Int)

class StandardClass

object StandardClass

class StandardClassWithCaseCompanion

case object StandardClassWithCaseCompanion