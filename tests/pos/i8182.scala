object example:

  trait Show[-A]:
    extension (a: A) def show: String

  given str (using rec: Show[String]): Show[String] = ??? // must be Show[String] as the argument

  given laz (using rec: => Show[String]): Show[Option[String]] = ??? // must be byname argument

  def test = Option("").show
