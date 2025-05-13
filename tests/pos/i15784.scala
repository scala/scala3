//> using options -Werror

def case1 = List(42) match
  case List(_, rest @ _*) => rest
  case _ => ???

def case2 = List(42) match
  case List(_, Rest @ _*) => Rest
  case _ => ???

def case3 = List(42) match
  case List(_, `Rest` @ _*) => Rest
  case _ => ???

def case4 = 42 match
  case `type` : Int => `type`

def case5 = 42 match
  case X @ (_: Int) => 32
