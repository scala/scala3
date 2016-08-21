trait T(x: Int) {
  def f = x
}

class C0 extends T // error

class C(x: Int) extends T() // error

trait U extends C with T

trait V extends C(1) with T(2) // error // error

trait W extends T(3) // error


class E extends T(0)
class F extends E with T(1) // error

