import scala.quoted.*

def macroImpl(using Quotes) =
  val t = summon[Type[Int]]
  Type.of[Int] match
    case '[t.Underlying] => '{true}
    case _ => '{false}
