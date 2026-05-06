
//> using options -Wsafe-init -Ysafe-init-global

import scala.compiletime.deferred

trait MyCodec[E]

object auto:
  trait CompanionEssentials[E]:
    given MyCodec[E] = deferred

import auto.CompanionEssentials

case class Person(name: String, age: Int)
object Person extends CompanionEssentials[Person]: // error
  //override final lazy given given_MyCodec_E: MyCodec[Person] = Person.given_MyCodec_E
  override def toString = ""
