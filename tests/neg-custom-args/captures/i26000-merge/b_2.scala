package i26000merge

import language.experimental.captureChecking

def misuse(q: File^) =
  Lib.m.value(y) // error
  Lib.mp.value[{q}](y) // error
  Lib.mhi.value[{y}](y) // error
  Lib.mlo.value[{a}](a) // error
  Lib.mboth.value[{}](File()) // error
  Lib.mboth.value[{a, z}](a) // error
  Lib.w.value[{}](q) // error
