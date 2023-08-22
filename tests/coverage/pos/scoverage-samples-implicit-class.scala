// minimized example from sbt-scoverage-samples
package org.scoverage.samples

implicit class StringOpssssss(s: String) {
  def ! (str: String): Unit = println(s + "!" + str)
}

class CreditEngine {
  def receive: Int => Unit = { req =>
    if (req < 2000)
      "if 1" ! "xd"
    else println("else 1")
  }
}
