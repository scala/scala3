import scala.{collection => coll, runtime=>_, _}
import coll._

def f(xs: Int*) = xs.sum
def test =
  f(List(1, 2, 3): _*)

def g = { implicit x: Int =>
  x + 1
}

def foo(x: Int) = x
def testTrailingUnderscoreEtaExpansion = foo _
