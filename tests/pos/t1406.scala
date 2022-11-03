
class Identifiers {

  def f(x: Any): Boolean = x match {
    case 𐐨XYZ: String => true
    case 𐐨                => true
  }
  def g(x: Any) = x match {
    case 𐐨 @ _ => 𐐨
  }
}
class Ops {
  def 𝆗 = 42        // was error: illegal character
  def op_𝆗 = 42     // was error: illegal character
  def 🌀 = 42
  def op_🌀 = 42
  def 🚀 = 42
  def op_🚀 = 42
  def 🜀 = 42
  def op_🜀 = 42
  def 𝓅 = 42
  def op_𝓅 = 42
}
class Strings {
  implicit class Interps(sc: StringContext) {
    def 𝓅(parts: Any*) = "done"
  }
  def 𝓅 = 42
  def interpolated = s"$𝓅"
  def e = "a 𝓅 b"
  def f = 𝓅"one"
}
