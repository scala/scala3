package i26000sep

import language.experimental.captureChecking
import caps.*

def leak(y: File^): File = Lib.f.value[{y}](y) // error
