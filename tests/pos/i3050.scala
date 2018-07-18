object Test {
  transparent def openImpl(): Int =
    Some(42) match { case Some(i) => i }

  def open(): Int = openImpl()

  transparent def openImpl2(): Int =
    Some(42) match { case None => 42 }

  def open2(): Int = openImpl2()
}
