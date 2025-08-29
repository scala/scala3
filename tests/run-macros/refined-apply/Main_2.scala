//> using options -experimental

import scala.quoted._
def testInt(i: Int) = ()
@main def Test(): Unit =
  val withType = Macro.transparentInlineCall()
  summon[withType.test1 <:< Int]
  withType.test2.map(testInt)
  Macro.inlineCall()
