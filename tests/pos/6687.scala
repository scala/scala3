type T[X] = X match {
  case String => Int
  case Int => String
}

class Box[X](x: X)

def f[X](x: Box[X]): T[X] = x match {
  case x: Box[Int]    => ""
  case x: Box[String] => 1
}
