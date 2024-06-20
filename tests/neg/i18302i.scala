def polyFun1: Option[PolyFunction] = ??? // error
def polyFun2: PolyFunction & Any = ??? // error
def polyFun3: Any & PolyFunction = ??? // error
def polyFun4: PolyFunction | Any = ??? // error
def polyFun5: Any | PolyFunction = ??? // error
def polyFun6(a: Any | PolyFunction) = ??? // error
