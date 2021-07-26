import annotation.varargs

trait C {
  @varargs def v(i: Int*) = ()
}

class D extends C { // error: name clash between defined and inherited member
  def v(i: Array[Int]) = ()
}