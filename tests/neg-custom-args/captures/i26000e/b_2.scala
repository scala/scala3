package i26000e

import language.experimental.captureChecking

def leak(y: File^): File =
  Lib.pick[{y}](() => y) // error
  Lib.keep.elem
