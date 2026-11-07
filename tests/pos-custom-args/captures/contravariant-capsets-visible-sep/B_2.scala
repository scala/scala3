package q
import language.experimental.captureChecking
import p.*

def use(io: File^) =
  O.mk(io)(io)
  O.mkSome(io).get(io)
  O.hs.head(io)
  O.mkAny().get(io)
