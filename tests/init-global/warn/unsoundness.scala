class Box[T](val value: T)

abstract class Base[T]:
  def update(n: T): Unit

class A[T](var a: T) extends Base[T]:
  def update(n: T) =
    a = n

class B[T](var b: T) extends Base[T]:
  def update(n: T) =
    O.x       // warn
    b = n

object O:
  val m: Int = 3
  f(if m > 5 then Box(A(3)) else Box(B(4)))

  val x: Int = 10

  def f(a: Box[Base[Int]]): Unit =
    h(a.value)

  def h(a: Base[Int]): Unit =
    a.update(10)