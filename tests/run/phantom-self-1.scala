object Test {
    def main(args: Array[String]): Unit = {
      Boo.any
      Boo.any2
    }
}

object Boo extends Phantom with T

trait T { self: Phantom =>
  type X = self.Any
  def any: X = self.assume
  def any2: X = assume
}
