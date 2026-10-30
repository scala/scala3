package i26000b

def leak(y: File^): File = Lib.f.value[{y}](y) // error

def useG(y: File^) =
  Lib.g.value(io)
  Lib.g.value(y) // error

def useH(y: File^) =
  Lib.h.value[{io}](io)
  Lib.h.value[{y}](y) // error
