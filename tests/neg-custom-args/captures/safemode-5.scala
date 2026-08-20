package test
import language.experimental.safe

object Test:
  def h(x: Any) = x.asInstanceOf[String] // error