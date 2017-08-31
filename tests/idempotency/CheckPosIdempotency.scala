
object Test {
  def main(args: Array[String]): Unit =
    IdempotencyCheck.checkIdempotency("../out/posIdempotency1", "../out/posIdempotency2")
}
