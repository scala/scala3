object Test:
  def one: Unit =
    for
      // Was already an error as expected
      (i, Some(_)) <- List.empty[Int] zip List.empty[Option[String]] // error
    do ()

  def two: Unit =
    for
      // Used to be a warning
      (i, Some(_)) <- List.empty[Int] lazyZip List.empty[Option[String]] // error
    do ()
