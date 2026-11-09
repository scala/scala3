type Decode[S <: String] = S match
  case "S" => String
  case "B" => Boolean

case class Collection[S <: String, T <: Decode[S]](
  ts: List[T],
)

class ClassyCollection[S <: String, T <: Decode[S]](
  val ts: List[T],
)
