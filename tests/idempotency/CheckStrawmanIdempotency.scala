
object Test {
  def main(args: Array[String]): Unit = {
    IdempotencyCheck.checkIdempotency("../out/idempotency/strawman0", "../out/idempotency/strawman1")
    // FIXME: #2964 and maybe more
    /*
    IdempotencyCheck.checkIdempotency("../out/idempotency/strawman1", "../out/idempotency/strawman2")
    IdempotencyCheck.checkIdempotency("../out/idempotency/strawman1", "../out/idempotency/strawman3")
    */
  }
}
