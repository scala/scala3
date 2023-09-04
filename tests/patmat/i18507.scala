import scala.quoted.*

trait DFValOf[T]

def calcWidth(x: List[Type[?]])(using q: Quotes): Unit =
  x.collect { case '[DFValOf[t]] => ???}
