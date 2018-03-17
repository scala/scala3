object App {
  trait A { type L >: Any}
  def upcast(a: A, x: Any): a.L = x
  lazy val p: A { type L <: Nothing } = p
  val q = new A { type L = Any }
  def coerce1(x: Any): Any = upcast(q, x)  // ok
  def coerce3(x: Any): Any = upcast(p, x)  // ok, since dependent result type is not needed
}
