enum Tag[A]:
  case MyTuple extends Tag[(String, String)]

def printIt[A](t: Tag[A], a: A): Unit =
  t match
    case Tag.MyTuple => println(a._1)

enum Tag2[A]:
  case MyTuple extends Tag2[String *: String *: EmptyTuple]

def printIt2[A](t: Tag2[A], a: A): Unit =
  t match
    case Tag2.MyTuple => println(a._1)
