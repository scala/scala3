package test

// IMPORTANT: this object has undefined behavior due to its illegal use of a $ in an identifier
//            This test is only to check that the ad-hoc workaround for Amonite `object $` is working.
//            If at some point it becomes impossible to support this test, we can drop it.
//            Ideally Amonite will have deprecated the `object $` by then.
object $ {
  def test(): Int = 1
}
