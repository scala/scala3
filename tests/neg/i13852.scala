inline val `1`: 1 = 1
def get1: 1 = `1`

opaque type One = 1
inline val One: One = 1 // error
def getOne: One = One
