val foo: Int => Int = Some(7) match
  case Some(y) => x => y
  case None => identity[Int]
