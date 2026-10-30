package i26000boxed

import language.experimental.captureChecking

def misuse(q: File^) =
  Lib.up.value[File^{q}](q) // error
  Lib.dep.value[{}, File^{q}](q) // error
  Lib.xs.value(List(q)) // error
  Lib.ub.value[List[File^{q}]](List(q)) // error
  Lib.mk(q).value[File^{io}](io) // error
