import scala.quoted.{_, given}
def test(given QuoteContext) = {
  val a = '{
    def z: Int = 5
    Macro.ff(z, 5)
  }
}
