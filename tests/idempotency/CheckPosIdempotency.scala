
object Test {
  def main(args: Array[String]): Unit =
    IdempotencyCheck.checkIdempotency("out/idempotency/posIdempotency1", "out/idempotency/posIdempotency2")
}
