package b

import a.*

object C:
  def m: Int =
    val a = new A()
      .setConnectTimeout(1)
      .setFailedAttempts(1)
    0
