package imptwice

abstract class foo(s: String);

object Util {
  def foo(s: String) = new foo(s) {}
}

import imptwice.Util.*


object User {
  def main(args: Array[String]) = {
    foo("blah")
  }
}
