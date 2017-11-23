object Test {
    def main(args: Array[String]): Unit = {
      foo(Boo.any)
      foo(Boo.any2)
    }
  def foo(unused a: Boo.X) = ()
}

object Boo extends Phantom with T

trait T { self: Phantom =>
  type X = self.Any
  unused def any: X = self.assume
  unused def any2: X = assume
}
