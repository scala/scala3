sealed trait Pretty { self: Color => }
sealed trait Dull { self: Color => }
enum Color  {
  case Pink extends Color with Pretty
  case Red extends Color with Dull
}

def describe(c: Color) = c match {
  case Color.Pink => "Amazing!"
  case Color.Red => "Yawn..."
}

def describe2(c: Pretty) = c match {
  case Color.Pink => "Amazing!"
}

def describe3(c: Dull) = c match {
  case Color.Red => "Yawn..."
}
