abstract class HasId(var id: String)

case class Entity(override val id: String) extends HasId(id) // error

object Test extends App {
  val entity = Entity("0001")
  entity.id = "0002"
  println(entity.id)
}
