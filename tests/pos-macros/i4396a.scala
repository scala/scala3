import scala.quoted.*
def test(using Quotes) = {
  '{ Option(4) match { case Some(a) => a; case None => 1 }}
}
