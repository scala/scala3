package i26000merge

import language.experimental.captureChecking

def use(q: File^) =
  Lib.m.value(a)
  Lib.mp.value[{q}](q)
  Lib.mhi.value[{a}](a)
  Lib.mlo.value[{a, y}](a)
  Lib.mboth.value[{a}](a)
  Lib.mboth.value[{a, y}](y)
  Lib.ml.head(a)
  Lib.w.head[{q}](q)
