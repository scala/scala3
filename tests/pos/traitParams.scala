trait T(x: Int) {
  def f = x
}

trait U extends T

class C extends U with T(2) {

}

class D extends C with T

