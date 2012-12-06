package dotty.tools.dotc
package core

import Types._

object Constants {

  case class Constant(value: Any) {
    def tpe: Type = ???
  }

}