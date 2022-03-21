abstract class HasId(var id: String)

case class Entity(override val id: String) extends HasId(id) // error

object Test extends App {
  val entity = Entity("0001")
  entity.id = "0002"
  println(entity.id)
}

trait HasId2:
  var id: String = ""

case class Entity2(override val id: String) extends HasId2 // error

trait HasId3:
  def id: String
  def id_=(x: String): Unit

case class Entity3(override var id: String) extends HasId3 // ok
