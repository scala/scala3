//> using options -Xfatal-warnings

import scala.annotation.switch

class Test {
  def unreachable(ch: Char) = (ch: @switch) match {
    case 'a' => println("b") // ok
    case 'a' => println("b") // warn: unreachable case
    case 'c' =>
  }
}

// nopos-error: No warnings can be incurred under -Werror.