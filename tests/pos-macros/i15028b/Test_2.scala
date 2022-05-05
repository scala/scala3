def test() =
  myTreeTraverse {
    println(1: Int)
    Option.empty[Int] match
      case Some(n) => 1
      case None => 5
  }
