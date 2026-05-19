//> using options --release 17
// test: -jvm 21+
// Must be tested where release target < current JVM.
// Maybe a bug that ct.sym is not used if release == JVM version.

import java.time.Instant

class C:
  def f: Instant = new Instant // error
  def g: Instant = Instant() // error
  def p: P = new P // error

class P private ()

@main def test() = println:
  C().f
