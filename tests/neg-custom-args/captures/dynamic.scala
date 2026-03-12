import scala.language.dynamics
import scala.language.experimental.safe

class User(map: Map[String, Any]) extends Dynamic { // error
  // selectDynamic is called when accessing obj.field
  def selectDynamic(name: String): Any = map.getOrElse(name, "Unknown")
}

def Test =
  val user = new User(Map("name" -> "Alice", "age" -> "30"))
  println(user.name) // Prints: Alice
  println(user.email) // Prints: Unknown
