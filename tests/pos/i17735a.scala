//> using options -Xfatal-warnings -Wvalue-discard -Wconf:msg=non-Unit:s

import scala.collection.mutable
import scala.annotation.nowarn

object Test:

  def f(b: Boolean): String =
    val messageBuilder = mutable.StringBuilder()
    if b then
      // Here @nowarn is effective with or without -Wfatal-warnings
      // i.e. no warning without -Wfatal-warnings and no error with -Wfatal-warnings
      messageBuilder.append("helloworld").append("\n")

    messageBuilder.result()

  def g(x: String => Unit) = ???
  def h: String =
    val messageBuilder = mutable.StringBuilder()
    g: s =>
      // here @nowarn is effective without -Wfatal-warnings (i.e. no warning)
      // But with -Wfatal-warnings we get an error
      messageBuilder.append("\n").append(s)
    messageBuilder.result()
