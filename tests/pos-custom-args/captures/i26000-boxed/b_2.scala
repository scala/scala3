package i26000boxed

import language.experimental.captureChecking

def use(q: File^) =
  Lib.up.value[File^{io}](io)
  Lib.dep.value[{q}, File^{q}](q)
  Lib.xs.value(List(io))
  Lib.ub.value[List[File^{io}]](List(io))
  Lib.mk(q).value[File^{q}](q)
