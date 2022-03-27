
def mustBeInstrumented = "literal"

def thisOneToo =
  println("not this") // this literal should not be instrumented, only the println call
  12 // this literal must be instrumented

def f(x: Int, y: Int, z: Int)(t: Int) = ???

def main: Unit =
  f(0,1,2)(3)
