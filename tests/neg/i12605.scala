object Foo:
  def joe(): List[(Int, Int)] =
    List((2, 3), (3, 4)).filter case (a, b) => b > a // error // error
