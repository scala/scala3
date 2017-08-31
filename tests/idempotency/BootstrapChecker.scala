
object Test {
  def main(args: Array[String]): Unit =
    IdempotencyCheck.checkIdempotency("../out/dotty1", "../out/dotty2")
}
