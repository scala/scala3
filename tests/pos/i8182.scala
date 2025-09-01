package example

trait Show[-A]:
  extension (a: A) def show: String

given Show[String] => Show[String] = ??? // must be Show[String] as the argument

given (rec: => Show[String]) => Show[Option[String]] = ??? // must be byname argument

def test = Option("").show
