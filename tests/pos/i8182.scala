package example

trait Show[-A]:
  extension (a: A) def show: String

given (using rec: Show[String]): Show[String] = ??? // must be Show[String] as the argument

given (using rec: => Show[String]): Show[Option[String]] = ??? // must be byname argument

def test = Option("").show
