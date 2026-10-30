package i26000merge

import language.experimental.captureChecking

object Use uses a:
  def use() =
    Lib.s.value(a)
    Lib.sp.value[{a}](a)
    Lib.m.value(a) // error
    Lib.mp.value[{a}](a) // error // error
    Lib.ml.head(a) // error
