//> using options -Wvalue-discard

import scala.collection.mutable
import scala.annotation.nowarn

object Foo:

  def f(b: Boolean): String =
    val messageBuilder = mutable.StringBuilder()
    if b then
      // Here @nowarn is effective with or without -Wfatal-warnings
      // i.e. no warning without -Wfatal-warnings and no error with -Wfatal-warnings
      messageBuilder.append("helloworld").append("\n"): @nowarn("msg=discarded non-Unit value*")

    messageBuilder.result()

  def g(x: String => Unit) = ???
  def h: String =
    val messageBuilder = mutable.StringBuilder()
    g: s =>
      // here @nowarn is effective without -Wfatal-warnings (i.e. no warning)
      // But with -Wfatal-warnings we get an error
      messageBuilder.append("\n").append(s): @nowarn("msg=discarded non-Unit value*")
    messageBuilder.result()