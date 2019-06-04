class JEnum {
  def name: String = "Foo"
  def ordinal: Int = 10
  def action = "fofofo"
}

enum A extends JEnum {
  case MONDAY, TUESDAY, SATURDAY
  case Stuff
  case Someday(x: String)
  def report = "Reported"
}

trait Foo1
trait Bar

enum B(val gravity: Double)(val isItGood: Boolean) extends Foo1 {
  case EARTH extends B(9.8)(true)
  case JUPITER extends B(100)(true)
  case MOON extends B(4.3)(true)
  case Foo extends B(10)(true) with Bar
}
