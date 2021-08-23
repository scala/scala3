object Test:
  type X = Int
  val x: X = 1

  type T2 = X match { case Int => String case String => Int }
  x match { case 1 => 3; case 2 => 4 }
  x match { case 1 => 3 case 2 => 4 }

  type T1 = X match { case Int => String; case String => Int } // error // error
