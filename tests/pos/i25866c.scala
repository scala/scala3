type Decode[S <: String] = S match
  case "S" => String
  case "B" => Boolean

type DecodeAlias[S <: String] = Decode[S]
type DecodeAlias2[S <: String] = DecodeAlias[S]   // double indirection

class C[S <: String, T <: DecodeAlias[S]](val ts: List[T]):
  val foo: List[? <: DecodeAlias[? <: String]] = ts

case class Coll[S <: String, T <: DecodeAlias[S]](ts: List[T])

class D[S <: String, T <: DecodeAlias2[S]](val ts: List[T]):
  val foo: List[? <: DecodeAlias2[? <: String]] = ts
