package app

import lib.*

case class Local(y: Int)
object Local extends HasOptions { val options = 1 }

trait Cli {
  Seq(Local, A, B, C, D, E)
}
