trait C extends (Int => 1)
class D extends (Int => 1) {
  def apply(x: Int) = 1
}
