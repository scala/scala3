import caps.any

def par(p1: () => Unit, p2: () => Unit) = ()
def seq(p1: () => Unit, p2: () ->{any, p1} Unit) = ()

def parCurriedBad(p1: () => Unit): (() => Unit) => Unit = // error: consume failure
  (p2: () => Unit) => par(p1, p2)
def parCurried(consume p1: () => Unit): (() => Unit) => Unit =
  (p2: () => Unit) => par(p1, p2)

type Proc = () => Unit

def test(c: () => Unit) =
  val p: Proc = ???
  par(p, p)  // error: separation
  seq(p, p)
  parCurriedBad(p)(p) // ok, but parCurriedBad ill-typed
  parCurried(p)(p) // error: consume failure

  val foo = (p1: () => Unit) => (p2: () ->{c, any} Unit) => par(p1, p2)
  foo(c)(c) // error: separation

  val bar = (p1: () => Unit) => (p2: () ->{p1, any} Unit) => par(p1, p2) // error, but error message could be better
  bar(c)(c)





