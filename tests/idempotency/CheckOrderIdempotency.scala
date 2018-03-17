
object Test {
  def main(args: Array[String]): Unit =
    IdempotencyCheck.checkIdempotency("out/idempotency/orderIdempotency1", "out/idempotency/orderIdempotency2")
}
