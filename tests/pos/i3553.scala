class Foo[T <: List[1]](param: List[1]) {
  def foo(in: List[1]): List[true] = Nil
  def bar[U >: List[1]](in: List['z']): List[1.0e9] = Nil
}
class Bar[T <: 1](param: 1) {
  def foo(in: 1): true = true
  def bar[U >: 1](in: 'z'): 1.0e9 = 1.0e9
}