import scala.compiletime.constValueTuple
import scala.deriving.Mirror

case class Hello(a: Int)
val mirror = summon[Mirror.Of[Hello]]
val test = constValueTuple[mirror.MirroredElemTypes] // error
