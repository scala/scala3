//> using target.platform scala-js

import scala.scalajs.js

def repro(str: js.UndefOr[String]): Unit =
  assert(str.nonEmpty)

object Test:
  def main(args: Array[String]): Unit =
    repro(js.defined("ok"))
