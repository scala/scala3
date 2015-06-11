trait T(x: Int) {
  def f = x
}

class C extends T  // error

trait U extends T

class D extends U {  // error

}

