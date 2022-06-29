import scala.compiletime.*
import scala.compiletime.ops.int.*

abstract sealed class HList :
  def ::[H](head: H): HNonEmpty[H, this.type] = HNonEmpty(head, this)

case object HNil extends HList
case class HNonEmpty[H, T <: HList](head: H, tail: T) extends HList :
  type Elem[N <: Int] =
    N match
      case 0 => H
      case S[n1] => Elem[n1]

  inline def apply[N <: Int](n: N): Elem[N] =
    n match
      case _: 0 => head
      case _: S[n1] => tail.asInstanceOf[HNonEmpty[?, ?]].apply(constValue[n1]) // error