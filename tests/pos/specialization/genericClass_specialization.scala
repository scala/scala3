object genericClass_specialization {
  class A[T] {
    def foo[@specialized(Int, Char, Double) U](b: U) = b
  }
  def foobar[@specialized(Char) X] = new A[X].foo(2)
}
