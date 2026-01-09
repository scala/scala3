
import scala.compiletime.deferred

trait MyCodec[E]

object auto:
  trait CompanionEssentials[E]:
    given myc: MyCodec[E] = deferred

import auto.CompanionEssentials

case class Person(name: String, age: Int)
object Person extends CompanionEssentials[Person]: // error
  given String = "hw"
  given myc(using String): MyCodec[Person] = new MyCodec[Person] {}
  override def toString = ""
