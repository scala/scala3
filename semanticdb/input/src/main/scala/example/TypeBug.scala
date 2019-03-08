package example

import scala.language.existentials
import scala.language.higherKinds

object TypBug {
  class M {
    def m: Int = ???
  }
  class C extends M {
    case class RepeatedType(s: String*) {
      def m1(x: Int*): Int = s.length
    }
  }
}
