def polyFun: PolyFunction { def apply: Int } = // error
  new PolyFunction { def apply: Int = 1 } // error
