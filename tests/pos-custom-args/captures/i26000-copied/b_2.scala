package i26000copied

import language.experimental.captureChecking

// The declared parameter types in the inferred types of definitions of another
// compilation unit are kept where typer copies them into inferred types
object Use uses io:
  val viaBm = Lib.bm
  val viaM = Box(Lib.m)
  def use(): Int = viaBm.value(io) + viaM.value(io)
