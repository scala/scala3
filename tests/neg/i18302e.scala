def polyFun: PolyFunction {  } = // error
  new PolyFunction {  }

def polyFun(f: PolyFunction {  }) = () // error
