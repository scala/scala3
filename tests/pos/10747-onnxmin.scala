import scala.compiletime.ops.string.+
import scala.compiletime.ops.int
import scala.compiletime.ops.int.{S, +, <, <=, *}
import scala.compiletime.ops.boolean.&&

object OnnxMin {
  type Index = Int & Singleton

  sealed trait Indices
  final case class :::[+H <: Index, +T <: Indices](head: H, tail: T) extends Indices

  sealed trait INil extends Indices
  case object INil extends INil


  type Dimension = Int & Singleton

  sealed trait Shape extends Product with Serializable

  final case class #:[+H <: Dimension, +T <: Shape](head: H, tail: T) extends Shape

  sealed trait SNil extends Shape
  case object SNil extends SNil

  type CContains[Haystack <: Indices, Needle <: Index] <: Boolean = Haystack match {
    case head ::: tail => head match {
      case Needle => true
      case _ => CContains[tail, Needle]
    }
    case INil => false
  }

  type AddGivenAxisSizeLoop[First <: Shape, Second <: Shape, AxisIndex <: Indices, I <: Index] <: Shape = First match {
    case head #: tail => CContains[AxisIndex, I] match {
      case true =>  Second match {
        case secondHead #: secondTail => head #: tail
        case SNil => AxisIndex match{
          case INil => SNil
        }
      }
    }
  }

  def ConcatV13: AddGivenAxisSizeLoop[Dimension #: Shape, Dimension #: Shape, Index ::: INil, 0] = ???
}
