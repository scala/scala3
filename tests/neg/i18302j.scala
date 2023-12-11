def polyFunByName: PolyFunction { def apply(thunk: => Int): Int } = // error
  new PolyFunction { def apply(thunk: => Int): Int = 1 } // error

def polyFunVarArgs: PolyFunction { def apply(args: Int*): Int } = // error
  new PolyFunction { def apply(thunk: Int*): Int = 1 } // error
