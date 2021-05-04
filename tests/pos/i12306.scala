class Record(elems: Map[String, Any]) extends Selectable:
  val fields = elems.toMap
  def selectDynamic(name: String): Any = fields(name)
object Record:
  def apply(elems: Map[String, Any]): Record = new Record(elems)
extension [A <: Record] (a:A) {
  def join[B <: Record] (b:B): A & B = {
    Record(a.fields ++ b.fields).asInstanceOf[A & B]
  }
}

type Person = Record { val name: String; val age: Int }
type Child = Record { val parent: String }
type PersonAndChild = Record { val name: String; val age: Int; val parent: String }

@main def hello = {
  val person = Record(Map("name" -> "Emma", "age" -> 42)).asInstanceOf[Person]
  val child = Record(Map("parent" -> "Alice")).asInstanceOf[Child]
  val personAndChild = person.join(child)

  val v1: PersonAndChild = personAndChild
  val v2: PersonAndChild = person.join(child)
}