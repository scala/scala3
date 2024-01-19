def test = polyFun(1)

def polyFun: PolyFunction { def apply(x: Int): Int } =
  new PolyFunction { def apply(x: Int): Int = x + 1 } // error
