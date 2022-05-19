package app

import scala.deriving.Mirror

object Main:
  def main(args: Array[String]): Unit =
    val mirrorTop = summon[Mirror.SumOf[lib.Top]]
    assert(mirrorTop ne lib.Top) // **NOT** cached in companion - previous run, pre-3.1 tasty dependency
    assert(mirrorTop.ordinal(lib.Middle()) == 0)
