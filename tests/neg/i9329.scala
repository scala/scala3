trait Parent {
  def name: String
}
trait Son extends Parent {
  abstract override def name = ""
  def parentName = super.name
}
class GrandSon extends Son // error