import scala.compiletime.constValueTuple
import scala.deriving.Mirror.ProductOf

case class C(date: Int, time: Int)

inline def labelsOf[A](using p: ProductOf[A]): Tuple = constValueTuple[p.MirroredElemLabels]

val headers: List[String] = labelsOf[C].toList.map(_.toString)
