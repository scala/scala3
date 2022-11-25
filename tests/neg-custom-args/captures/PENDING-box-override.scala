abstract class A[X]:
  def foo(x: X): X

class IO
class C
def test(io: {*} IO) =
  class B extends A[{io} C]:    // error, but should work
    override def foo(x: {io} C): {io} C = ???

/* -- Error: box-override.scala:7:8 -----------------------------------------------
7 |  class B extends A[{io} C]:
  |        ^
  |class B needs to be abstract, since def foo(x: X): X in class A is not defined
  |(Note that
  | parameter X in def foo(x: X): X in class A does not match
  | parameter {io} C in override def foo(x: {io} C): {io} C in class B
  | )
*/
