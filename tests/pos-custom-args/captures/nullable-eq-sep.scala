//> using options -Yexplicit-nulls
import language.experimental.captureChecking
import language.experimental.separationChecking

class Ref extends caps.ExclusiveCapability

def test1(a: (Ref | Null)^): Boolean = a eq a
def test2(a: (Ref | Null)^): Boolean = a ne a
def test3(a: (Ref | Null)^): Boolean =
  val b: (Ref | Null)^{a} = a
  a eq b
def test4(a: (Ref | Null)^): Boolean =
  val b: (Ref | Null)^{a} = a
  b eq a
