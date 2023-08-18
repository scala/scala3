
class Test1 {
  type A
  type B <: A

  def foo(): A = ???
  def foo(): A = ??? // error

  def bar(): A = ???
  def bar(): B = ??? // error
}

class Test2 {
  type A
  type B <: A

  def foo(x: A) = ???
  def foo(x: A) = ??? // error

  def bar(x: A) = ???
  def bar(x: B) = ??? // error
}
