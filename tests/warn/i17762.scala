//> using options -Werror -Wunused:all

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
