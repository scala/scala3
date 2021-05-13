import scala.quoted._
import scala.deriving._

def blah2[P <: Product, MEL <: Tuple: Type, MET <: Tuple: Type](m: Mirror.ProductOf[P] { type MirroredElemLabels = MEL; type MirroredElemTypes = MET})(using Quotes) = {
  Type.of[Tuple.Zip[MEL, MET]]
  ()
}
