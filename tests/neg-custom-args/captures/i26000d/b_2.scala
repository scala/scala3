package i26000d

def misuse(y: File^): File =
  Lib.mk(y).value((x: File) => x) // error
def misuse2(y: File^): File =
  Lib.mk2(y).value[{}]((x: File) => x) // error
