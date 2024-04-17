import scala.compiletime.ops.int.+
import scala.compiletime.ops.string.{CharAt, Length, Substring}
import scala.Tuple.*

type ArgTypes[S <: String] <: Tuple = S match
  case "" => EmptyTuple
  case _ =>
    CharAt[S, 0] match
      case '%' =>
        CharAt[S, 1] match
          case 'd' => Int *: ArgTypes[Substring[S, 2, Length[S]]]
          case 's' => String *: ArgTypes[Substring[S, 2, Length[S]]]
      case _ => ArgTypes[Substring[S, 1, Length[S]]]

def printf(s: String)(t: ArgTypes[s.type]): Unit = ()

def test() =
  printf("%s is %d")(("Ada", 36)) // works in Scala 3.2.0, 3.3.0 and 3.4.0
  printf("%s is lorem %d")(("Ada", 36)) // works in Scala 3.4.0 but fails in Scala 3.2.0 and 3.3.0
