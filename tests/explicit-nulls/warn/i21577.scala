def f(s: String) =
  val s2 = s.trim()
  s2 match
    case s3: String => println(1)
    case _ => println(2) // warn


def f2(s: String | Null) =
  val s2 = s.nn.trim()
    s2 match
      case s3: String => println(1)
      case _ => println(2) // warn

def f3(s: String | Null) =
  val s2 = s
    s2 match
      case s3: String => println(1)
      case _ => println(2) // warn

def f4(s: String | Int) =
  val s2 = s
    s2 match
      case s3: String => println(1)
      case _ => println(2)