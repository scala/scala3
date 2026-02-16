import scala.language.experimental.modularity

def f(x: Int): Int = x
def id[T](x: T): T = x
def idDependent(x: Any): x.type = x

def test =
  val v1: f(1) = f(1) // error
  val v2: id(1) = f(1) // error
  val v3: idDependent(1) = f(1) // error
