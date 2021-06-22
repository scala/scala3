@main def Test =
  println(null.isInstanceOf[Null])

  null match
    case _: Null => println("null")
    case _ => println("not null")