import language.experimental.captureChecking

// Match types that reduce to a boxed type argument are unboxed at use sites

class C:
  def use(): Unit = ()

type Fst[X] = X match
  case (t, u) => t

def fst[X](x: X): Fst[X] = ???
def lazyFst[X](x: X): () -> Fst[X] = ???

class Box[+T](val x: T)

type Unbox[X] = X match
  case Box[t] => t

def unbox[X](x: X): Unbox[X] = ???

def take(x: C^): Unit = ()

def test(c: C^) =
  val a: C^{c} = fst((c, 1))
  val b = fst((c, 1))
  val d: C^{c} = b
  fst((c, 1)).use()
  take(fst((c, 1)))
  val l: () ->{c} Unit = () => fst((c, 1)).use()
  val h: () ->{c} C^{c} = lazyFst((c, 1))
  val u: C^{c} = unbox(Box(c))
  unbox(Box(c)).use()

def testApply(c: C^) =
  val f = () => c.use()
  fst((f, 1))()
  val g: () ->{c} Unit = () => fst((f, 1))()
