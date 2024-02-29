def polyFun: PolyFunction {  } = // error
  new PolyFunction {  } // error

def polyFun(f: PolyFunction {  }) = () // error
