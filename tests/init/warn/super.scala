trait A:
  def foo() = 1

trait B:
  def foo() = 1

trait C:
  def foo() = n
  def n: Int

class Foo extends A, B, C:
  super[A].foo()

  override def foo() = n

  val n = 10

class Bar extends A, B, C:
  super[C].foo()

  override def foo() = n * n

  val n = 10  // warn

class Qux extends A, B, C:
  super.foo()

  override def foo() = n * n

  val n = 10 // warn
