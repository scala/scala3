type DoubleToString[D <: Double] <: String = D match
  case 0.0 => "0.0"
  case -0.0 => "-0.0"
  case _ => "_"

type DoubleToString2[D <: Double] <: String = D match
  case 0.0 => "0.0"
  case _ => "_"

type DoubleToString3[D <: Double] <: String = D match
  case -0.0 => "-0.0"
  case _ => "_"

type FloatToString[F <: Float] <: String = F match
  case 0.0f => "0.0f"
  case -0.0f => "-0.0f"
  case _ => "_"

type FloatToString2[F <: Float] <: String = F match
  case 0.0f => "0.0f"
  case _ => "_"

type FloatToString3[F <: Float] <: String = F match
  case -0.0f => "-0.0f"
  case _ => "_"

@main def main(): Unit = {
  summon[0.0 =:= 0.0]
  summon[-0.0 =:= -0.0]
  summon[DoubleToString[0.0] =:= "0.0"]
  summon[DoubleToString[-0.0] =:= "-0.0"]
  summon[DoubleToString[3.14] =:= "_"]
  summon[DoubleToString2[0.0] =:= "0.0"]
  summon[DoubleToString2[-0.0] =:= "_"]
  summon[DoubleToString3[-0.0] =:= "-0.0"]
  summon[DoubleToString3[0.0] =:= "_"]
  summon[0.0f =:= 0.0f]
  summon[-0.0f =:= -0.0f]
  summon[FloatToString[0.0f] =:= "0.0f"]
  summon[FloatToString[-0.0f] =:= "-0.0f"]
  summon[FloatToString[3.14f] =:= "_"]
  summon[FloatToString2[0.0f] =:= "0.0f"]
  summon[FloatToString2[-0.0f] =:= "_"]
  summon[FloatToString3[-0.0f] =:= "-0.0f"]
  summon[FloatToString3[0.0f] =:= "_"]
}
