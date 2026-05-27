import language.experimental.captureChecking

class Ref:
  def get(x: Int): Int = x

def badSingleton(a: Ref^, b: Ref^) =
  val f: Int ->{a} Int =
    x => b.get(x) // error

def badPair(a: Ref^, b: Ref^, c: Ref^) =
  val f: Int ->{a, b} Int =
    x => c.get(x) // error

def badTyped(a: Ref^, b: Ref^) =
  val f =
    ((x: Int) =>
      b.get(x) // error
    ): (Int ->{a} Int)
