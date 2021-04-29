object OK {
  def apply(n: Int ): Unit = ()
  def apply(n: Long): Unit = ()
  apply(3)  // ok
  apply(3L) // ok
}

object KO {
  type Key = Int
  def apply(n: Key ): Unit = ()
  def apply(n: Long): Unit = ()
  apply(3)  // error
  apply(3L) // ok
}
