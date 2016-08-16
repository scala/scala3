trait T(x: Int) {
  def f = x
}

trait U extends T

class D extends U {  // error

}

