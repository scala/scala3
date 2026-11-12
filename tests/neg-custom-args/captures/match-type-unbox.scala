import language.experimental.captureChecking

// Unboxing the result of a match type charges the boxed capture set

class C:
  def use(): Unit = ()

type Fst[X] = X match
  case (t, u) => t

def fst[X](x: X): Fst[X] = ???
def lazyFst[X](x: X): () -> Fst[X] = ???

def test(c: C^) =
  val a: C = fst((c, 1)) // error
  val l: () -> Unit = () => fst((c, 1)).use() // error
  val m: () -> C^{c} = () => fst((c, 1)) // error
  val h: () -> C^{c} = lazyFst((c, 1)) // error
  val x = fst((c, 1))
  val k: () -> Unit = () => x.use() // error

def testApply(c: C^) =
  val f = () => c.use()
  val g: () -> Unit = () => fst((f, 1))() // error
