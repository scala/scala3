import language.experimental.safe

val x: Any = 42
val y = x match
  case i: Int if i > 0 => s"positive int: $i"
  case i: Int => s"int: $i"
  case s: String => s"str: $s"
  case _ => "other"