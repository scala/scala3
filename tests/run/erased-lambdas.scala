//> using options -language:experimental.erasedDefinitions
// scalajs: --skip

// lambdas should parse and work

type F = (erased x: Int, y: String) => String
type S = (x: Int, erased y: String) => Int

def useF(f: F) = f(5, "a")
def useS(f: S) = f(5, "a")

val ff: F = (erased x, y) => y

val fs: S = (x, erased y) => x
val fsExpl = (x: Int, erased y: String) => x

// contextual lambdas should work

type FC = (x: Int, erased y: String) ?=> Int

def useCtx(f: FC) = f(using 5, "a")

val fCv: FC = (x, erased y) ?=> x
val fCvExpl = (x: Int, erased y: String) ?=> x

// nested lambdas should work

val nested: Int => (x: String, erased y: Int) => FC = a => (_, erased _) => (c, erased d) ?=> a + c

@main def Test() =
  assert("a" == useF(ff))

  assert(5 == useS(fs))
  assert(5 == useS(fsExpl))
  assert(5 == useS { (x, erased y) => x })

  assert(5 == useCtx(fCv))
  assert(5 == useCtx(fCvExpl))
  assert(5 == useCtx { (x, erased y) ?=> x })

  assert(6 == useCtx(nested(1)("b", 2)))
