import scala.quoted.*
def test(using Quotes) = {
  val q = Type.of[String]
  Type.of[String]
}
