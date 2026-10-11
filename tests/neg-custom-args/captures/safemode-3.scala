package test
import language.experimental.safe

object Test:

  def f(x: Any) = x match
    case x: List[Int @unchecked] => x.head // error
    case _ => 0

  def g(x: Any) = x match
    case x: List[?] => x.length
    case x: String => x.length
    case _ => 0

