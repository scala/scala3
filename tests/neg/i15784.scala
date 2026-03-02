def i15784 = List(42) match
  case List(_, Rest @ `a`) => Rest // error
  case List(_, Rest @ A) => Rest // error 
  case _ => ???

def case2 = 42 match
  case X: Int => X  // warn

def case3 = 42 match
  case `Int`: Int => `Int` // warn
