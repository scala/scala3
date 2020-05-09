trait S { type N; def n: N }

def newS[X](n: X): S { type N = X } = ???

def test =
  val ss: List[S] = ???
  val cl1 = (s: S) => newS(s.n)
  val cl2: (s: S) => S { type N = s.N } = cl1
  def f[R](cl: (s: S) => R) = cl
  val x = f(s => newS(s.n))
  val x1: (s: S) => S = x
    // If the code in `tptProto` of Namer that refers to this
    // file is commented out, we see:
    // pickling difference for the result type of the closure argument
    // before pickling: S => S { type N = s.N }
    // after pickling : (s: S) => S { type N = s.N }

  ss.map(s => newS(s.n))
    // If the code in `tptProto` of Namer that refers to this
    // file is commented out, we see a pickling difference like the one above.

  def g[R](cl: (s: S) => (S { type N = s.N }, R)) = ???
  g(s => (newS(s.n), identity(1)))

  def h(cl: (s: S) => S { type N = s.N }) = ???
  h(s => newS(s.n))

