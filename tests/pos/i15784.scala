def i15784 = List(42) match
  case List(_, rest @ _*) => rest
  case List(_, Rest @ _*) => Rest
  case List(_, `Rest` @ _*) => Rest
  case _ => ???

def i15784_auxiliary = 42 match
  case `type` : Int => `type`
