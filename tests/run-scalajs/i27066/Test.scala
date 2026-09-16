import scala.scalajs.js

def repro(str: js.UndefOr[String]): Boolean =
  assert(str.nonEmpty)
  str.nonEmpty

object Test:
  def main(args: Array[String]): Unit =
    println(repro(js.defined("ok")))
