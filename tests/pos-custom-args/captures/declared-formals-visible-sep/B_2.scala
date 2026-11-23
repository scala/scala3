package q
import language.experimental.captureChecking
import p.*

def use(a: File^, d: D, s: Sink[File^]) =
  O.h((f: File^) => ())
  O.v1(Ref[File^](a))
  O.v2(s)
  O.v3((f: File^) => ())
  O.v4[{a}]((f: File^{a}) => ())
  d.v5((f: File^{d.x}) => ())
