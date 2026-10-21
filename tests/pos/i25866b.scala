type Decode[S <: String] = S match
  case "S" => String
  case "B" => Boolean

class Collection[S <: String, T <: Decode[S]](val ts: List[T]):
  val foo: List[? <: Decode[? <: String]] = ts
