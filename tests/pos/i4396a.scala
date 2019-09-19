import scala.quoted.{_, given}
def test(given QuoteContext) = {
  '{ Option(4) match { case Some(a) => a; case None => 1 }}
}
