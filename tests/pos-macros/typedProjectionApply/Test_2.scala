import scala.quoted._
def test() =
  Macro.transparentInlineCall()
  Macro.inlineCall()
