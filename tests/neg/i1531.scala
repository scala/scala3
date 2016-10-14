trait T {
  def f: Int
}

class A(f: Int) extends T // error: class A needs to be abstract

