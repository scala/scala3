val foo: Int => Int = Option(7) match
  case Some(y) => x => y
  case None => identity[Int]
