trait partial_specialization {
  def foo1stOutOf1[@specialized(Int, Char) T](t: T) = ???
  def foo1stOutOf2[@specialized(Int, Char) T, U](t: T, u: U): T = t
  def foo2ndOutOf2[T, @specialized(Int, Char) U](t: T, u: U) = ???
  def fooAllOutOf2[@specialized(Int, Char) T, @specialized(Int, Char) U](t: T, u: U) = ???
  def foo1st3rdOutOf3[@specialized(Int, Char) T, U, @specialized(Int, Char) V](t: T, u: U, v: V) = ???

  def main(args: Array[String]) = {
    foo1stOutOf2(1, 2.0)
    foo1stOutOf2(1, 2.0)
    foo1st3rdOutOf3(1, 2, 'c')
    foo2ndOutOf2(1, 'c')
    fooAllOutOf2('a','b')
    fooAllOutOf2(1.0,1.0)
  }
}