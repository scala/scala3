package app

import scala.deriving.Mirror

object Main:
  def main(args: Array[String]): Unit =
    val mirrorTop = summon[Mirror.SumOf[lib.Top]]
    assert(mirrorTop.ordinal(lib.Middle()) == 0)
