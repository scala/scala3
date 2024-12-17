package dependentAnnotation

class lambdaAnnot(g: () => Int) extends annotation.StaticAnnotation

def f(x: Int): Int @lambdaAnnot(() => x + 1) = x

@main def main =
  val y: Int = 5
  val z = f(y)
