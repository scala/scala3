
object Test {
  def main(args: Array[String]): Unit =
    IdempotencyCheck.checkIdempotency("out/tastyBootstrap/dotty1", "out/tastyBootstrap/dotty2")
}
