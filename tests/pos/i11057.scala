object data {

    trait OfType[T]
    case object IntT extends OfType[Int]
    case object DoubleT extends OfType[Double]
    case object FloatT extends OfType[Float]

    type DSeq[X] = scala.collection.immutable.AbstractSeq[X]

    case class ColumnName[T](n:String, t: OfType[T])
    case class Column[T,F[_]<:DSeq[_]](n:F[T], of: ColumnName[T])
  }

  def min4[T,F[_]<:data.DSeq[T]](col: data.Column[T,F])(using Ordering[T]): T = {
    col match {
      case c:data.Column[Int,_] => c.n.min[T](Ordering[T])
      case _:data.Column[Double,_] => ???
      case _:data.Column[Float,_] => ???
    }
  }
