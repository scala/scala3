import scala.quoted._
def test(using Quotes) = {
  '{ Option(4) match { case Some(a) => a; case None => 1 }}
}
