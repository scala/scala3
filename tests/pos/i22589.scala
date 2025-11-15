
import scala.compiletime.deferred

trait MyCodec[E]

object auto:
  trait CompanionEssentials[E]:
    //given [E] => MyCodec[E] = deferred
    given MyCodec[E] = deferred

import auto.CompanionEssentials

case class Person(name: String, age: Int)
object Person extends CompanionEssentials[Person]:
  //given something: [E] => MyCodec[E] = new MyCodec[E] {}
  given something: MyCodec[Person] = new MyCodec[Person] {}
  override def toString = ""
