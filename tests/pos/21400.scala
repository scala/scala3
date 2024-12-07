//> using options -Ykind-projector:underscores

import scala.compiletime.ops.int.S

type IndexOf[T <: Tuple, E] <: Int = T match
  case E *: _  => 0
  case _ *: es => 1 // S[IndexOf[es, E]]
