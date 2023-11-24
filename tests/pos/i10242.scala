//> using options -source:3.3

// https://github.com/lampepfl/dotty/issues/10242
type Foo[A, B <: A] = A

type Bar[A] = A match {
  case Foo[String, x] => Unit
}

import scala.compiletime.ops.int.*

sealed trait HList
final case class HCons[H <: Int & Singleton, T <: HList](head: H, tail: T)
    extends HList
sealed trait HNil extends HList
case object HNil extends HNil

sealed trait Tensor[T, S <: HList]

object tf:
  def zeros[S <: HList](shape: S): Tensor[Float, S] = ???

  type NumElements[X <: HList] <: Int = X match
    case HNil              => 1
    case HCons[head, tail] => head * NumElements[tail]

  def reshape[T, From <: HList, To <: HList](
      tensor: Tensor[T, From],
      shape: To
  )(using NumElements[From] =:= NumElements[To]): Tensor[T, To] = ???

object test:
  val x = HCons(1, HCons(2, HNil))
  val y = tf.reshape(tf.zeros(x), HCons(2, HCons(1, HNil)))
