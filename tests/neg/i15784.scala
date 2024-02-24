def i15784 = List(42) match
  case List(_, Rest @ `a`) => Rest // error
  case List(_, Rest @ A) => Rest // error 
  case _ => ???