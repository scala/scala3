package covtest

object StructuralTypes:

  case class Record(elems: (String, Any)*) extends Selectable:
    def selectDynamic(name: String): Any = elems.find(_._1 == name).get._2

  type Person = Record {
    val name: String
  }

  def test(): Unit =
    val person = Record("name" -> "Emma", "age" -> 42).asInstanceOf[Person]
    person.name
