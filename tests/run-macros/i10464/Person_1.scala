trait Person with
  def name: String

case class PersonA(name: String) extends Person
case class PersonB(name: String) extends Person
