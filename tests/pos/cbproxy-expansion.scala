//> using options -language:experimental.modularity -source future
trait TC[T]:
  type Self

def f1[S, T: TC[S] as tc](x: S, y: tc.Self) = ()
def f2[S, T: TC[S]](x: S, y: T.Self) = ()
def f3[S, T: TC[S]](x: S, y: Int) = ()

given TC[String] with
  type Self = Int
  def unit = 42

def main =
  f1("hello", 23)
  f2("hello", 23)
  f3("hello", 23)
