package i26000copied

import language.experimental.captureChecking

object Use2 uses io:
  def use(): Int = Use.viaBm.value(io) + Use.viaM.value(io)
