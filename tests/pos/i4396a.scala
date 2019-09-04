import scala.quoted._
def test given QuoteContext = {
  '{ Option(4) match { case Some(a) => a; case None => 1 }}
}
