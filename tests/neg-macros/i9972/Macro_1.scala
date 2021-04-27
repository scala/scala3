package notmacro

import scala.util.NotGiven

object Main extends App {
  summon[NotGiven[T[Int]]] // error
}
