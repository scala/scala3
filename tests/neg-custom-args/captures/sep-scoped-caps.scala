class A
class B
class S extends caps.SharedCapability
import caps.fresh

def test(io: Object^): Unit =
  val h: S -> B^ = ???
  val _: (x: S) -> B^{fresh} = (x: S) => h(x)  // error: eta expansion fails since `h` is non-local
