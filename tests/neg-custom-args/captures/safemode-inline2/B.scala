import language.experimental.safe
import A.*

object Test:
  val _ = g2("a".asInstanceOf[Int]) // error
  val _ = g2("a".asInstanceOf[Int]) // error
