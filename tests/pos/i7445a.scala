// https://github.com/lampepfl/dotty/issues/7445

object Main {
  type O1[A] = {
    type OutInner[X] = Unit
    type Out = OutInner[A]
  }

  def f1: O1[Int]#Out = ???
}
