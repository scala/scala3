def test() =
  myTreeTraverse {
    Option.empty[Int] match
      case Some(n) => 1
      case None => 5
  }
