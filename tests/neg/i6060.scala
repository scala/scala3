class I1(i2: Int) {
  def apply(i3: Int) = 1
  new I1(1)(2) {} // error: too many arguments in parent constructor
}

class I0(i1: Int) {
  class I0[I2] {
    def apply(i3: Int) = 1
    new I0[Int]()(2) {} // error: too many arguments in parent constructor
  }
}