object Extract {
  transparent inline def unapply(value: String): Option[Tuple] = Some((1, "two"))
}
def fail(): Unit = "" match { case Extract(a, b) => f(a, b) }
def f(n: Int, s: String): Unit = ()
