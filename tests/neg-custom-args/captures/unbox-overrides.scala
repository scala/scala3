import caps.unbox

trait A[X]:
  def foo(@unbox x: X): X
  def bar(x: X): X

trait B extends A[C]:
  def foo(x: C): C        // error
  def bar(@unbox x: C): C // error

trait B2:
  def foo(x: C): C
  def bar(@unbox x: C): C

abstract class C extends A[C], B2 // error
