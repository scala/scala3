type Decode[S <: String] = S match
  case "S" => String

type DecodeAlias[S <: String] = Decode[S]

class C[S <: String, T <: DecodeAlias[S]](val ts: List[T]):
  val foo: List[? <: DecodeAlias[? <: String]] = ts
