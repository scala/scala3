import scala.compiletime.ops.int.S

sealed trait TList
sealed trait TNil extends TList
sealed trait ++:[H[_], T <: TList] extends TList

type IndexOf[H[_], T <: TList] <: Int = T match
  case H ++: _ => 0
  case _ ++: t => S[IndexOf[H, t]]

// compiles fine
val a = summon[ValueOf[IndexOf[List, List ++: Option ++: TNil]]].value

// causes an error
val b = summon[ValueOf[IndexOf[List, Option ++: List ++: TNil]]].value

object T extends App:
  println(a)
