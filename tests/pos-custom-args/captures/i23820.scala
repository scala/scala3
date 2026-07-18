import language.experimental.captureChecking

class Ref:
  def get(x: Int): Int = x

def okSingleton(a: Ref^) =
  val f: Int ->{a} Int =
    x => a.get(x)

def okPair(a: Ref^, b: Ref^) =
  val f: Int ->{a, b} Int =
    x => a.get(b.get(x))

def okTyped(a: Ref^) =
  val f =
    ((x: Int) =>
      a.get(x)
    ): (Int ->{a} Int)
