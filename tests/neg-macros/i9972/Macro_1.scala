package notmacro

import scala.util.Not

object Main extends App {
  summon[Not[T[Int]]] // error
}
