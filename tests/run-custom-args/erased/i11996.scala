final class UnivEq[A]

object UnivEq:
  erased def force[A]: UnivEq[A] =
    compiletime.erasedValue

extension [A](a: A)
  inline def ==*[B >: A](b: B)(using erased UnivEq[B]): Boolean = a == b
  inline def !=*[B >: A](b: B)(using erased UnivEq[B]): Boolean = a != b

case class I(i: Int)

@main def Test = {
  def test[A](a: A, b: A): Unit = {
    erased given UnivEq[A] = UnivEq.force[A]
    println(a ==* a)
    println(a !=* b)
  }
  println("Test starting...")
  test(I(1), I(2)) // error
  test(1, 2)
  test(true, false)
}
