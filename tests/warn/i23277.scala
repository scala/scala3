
enum Test:
  case One
  case Two(i: Int)

object Test:
  object Two:
    def apply(i: Int): Test.Two = Test.Two(i) // warn
    def apply(i: Int, j: Int): Test.Two = new Test.Two(i+j)
    def apply(i: Int, s: String): Two = Two(i, s) // warn because unprefixed Two is deemed pure
    def apply(): Test.Two = other.apply() // nowarn prefix is method call
    def other: this.type = this

object R extends Runnable:
  def r: this.type = this
  override def run() = r.run()

final class C(c: C):
  def f(i: Int): Int = c.f(i)

@main def main = println:
  Test.Two(1)
