import caps.use

trait A[X]:
  def foo(x: X @use): X
  def bar(x: X): X

trait B extends A[C]:
  def foo(x: C): C        // ok
  def bar(x: C @use): C // error

trait B2:
  def foo(x: C): C
  def bar(x: C @use): C

trait B3 extends B2:
  def foo(x: C @use): C // error
  def bar(x: C): C // ok

abstract class C extends A[C], B2 // error

trait Poly:
  def foo[T](a: Int)(x: C): C
  def bar[T](a: Int)(x: C @use): C

trait Poly1 extends Poly:
  def foo[T](a: Int)(x: C @use): C // error
  def bar[T](a: Int)(x: C): C // ok

trait D1:
  def foo(x: (List[C @use], List[C])): Unit
  def bar(x: (List[C @use], List[C @use])): Unit
  def baz(x: (List[C], List[C]) @use): Unit

trait D2 extends D1:
  def foo(x: (List[C], List[C]) @use): Unit  // error
  def bar(x: (List[C], List[C]) @use): Unit  // error
  def baz(x: (List[C] @use, List[C] @use)): Unit  // ok

trait E2:
  def foo(x: (List[C], List[C]) @use): Unit
  def bar(x: (List[C], List[C]) @use): Unit
  def baz(x: (List[C] @use, List[C] @use)): Unit

trait E3 extends E2:
  def foo(x: (List[C] @use, List[C])): Unit  // ok
  def bar(x: (List[C], List[C])): Unit  // ok
  def baz(x: (List[C] @use, List[C] @use)): Unit  // ok

