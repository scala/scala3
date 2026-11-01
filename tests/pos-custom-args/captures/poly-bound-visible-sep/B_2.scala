package q
import language.experimental.captureChecking
import caps.*
import p.*

def use(x: File^) =
  O.pg.value[{x}](File())
  O.mk().value[{x}](File())
  O.mkInline().value[{x}](File())
