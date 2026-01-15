//> using options -Wunused:all

class SomeType

def testIt(st1: SomeType, st2: SomeType): Boolean =
  given CanEqual[SomeType, SomeType] = CanEqual.derived
  st1 == st2

object HasCanEqual:
  given f: CanEqual[SomeType, SomeType] = CanEqual.derived

object UsesCanEqual:
  import HasCanEqual.given
  def testIt(st1: SomeType, st2: SomeType): Boolean =
    st1 == st2

object UsesCanEqual2:
  import HasCanEqual.f
  def testIt(st1: SomeType, st2: SomeType): Boolean =
    st1 != st2

object UsesCanEqual3:
  import HasCanEqual.f as g
  def testIt(st1: SomeType, st2: SomeType): Boolean =
    st1 != st2

def warnable(st1: SomeType, st2: SomeType): Boolean =
  given CanEqual[SomeType, SomeType] = CanEqual.derived // warn
  st1.toString == st2.toString

def importable(st1: SomeType, st2: SomeType): Boolean =
  import HasCanEqual.given // warn
  st1.toString == st2.toString
