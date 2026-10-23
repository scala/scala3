package i26000b

def leak(y: File^): File = Lib.f.value[{y}](y) // error
