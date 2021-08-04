import scala.quoted._
import scala.deriving._

def blah[P <: Product]
    (m: Mirror.ProductOf[P])
    (using Quotes, Type[m.MirroredElemLabels], Type[m.MirroredElemTypes]) = {
  type z = Tuple.Zip[m.MirroredElemLabels, m.MirroredElemTypes]
  Type.of[z] // error
  ()
}
