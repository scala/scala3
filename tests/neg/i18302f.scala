def polyFun: PolyFunction = // error
  new PolyFunction {  } // error

def polyFun2(a: PolyFunction) = () // error

val polyFun3: PolyFunction = // error
  new PolyFunction {  } // error
