import scala.quoted._
def test given QuoteContext = {
  val a = '{
    def z: Int = 5
    Macro.ff(z, 5)
  }
}
