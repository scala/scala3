trait Z {
  type Q
  def test: Q
}
class X(val x: Z)
class Y(x: Z) extends X(x) {
  x.test
}
