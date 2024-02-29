def test = polyFun(1)(2)

def polyFun: PolyFunction { def apply(x: Int)(y: Int): Int } = // error
  new PolyFunction: // error
    def apply(x: Int)(y: Int): Int = x + y
