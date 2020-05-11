import scala.quoted._
def test(using Scope) = {
  val a = '{
    def z: Int = 5
    Macro.ff(z, 5)
  }
}
