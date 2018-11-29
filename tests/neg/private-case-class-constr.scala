case class Nat private (value: Int)
object Nat {
  def apply(value: Int, disable: Boolean): Option[Nat] =
    if (value < 0 || disable) None else Some(new Nat(value))
}
object Test {
  val n1o = Nat(2) // error
  val n2o = Nat(2, false) // ok
  for (n <- n2o) yield n.copy() // error
}