
class Foo {
  val bar = new Bar {
    type S = Int
    type T = Int => Int
    type U = [X] =>> Int
    val x: Long = 2L
    def y: Boolean = true
    def z(): Char = 'f'
    def z2()(): Char = 'g'
    def w[T]: String = "a"
    def w2[T](a: Null)(b: Null): Null = null
  }
}

trait Bar {
  type S
  type T
  type U <: [X] =>> Any
  val x: Any
  def y: Any
  def z(): Any
  def z2()(): Any
  def w[T]: Any
  def w2[T](a: Null)(b: Null): Any
}
