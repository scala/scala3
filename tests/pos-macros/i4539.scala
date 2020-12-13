import scala.quoted._
def test(using Quotes) = {
  val q = Type.of[String]
  Type.of[String]
}
