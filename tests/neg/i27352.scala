val xss = Vector(Vector(Some(1)))

def compilerCrash: String =
  for xs <- xss yield
    xs.map: _ match // error
        case None => " "
        case 1    => "1"
