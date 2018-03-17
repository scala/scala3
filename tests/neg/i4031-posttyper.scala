object App {
  trait A { type L >: Any}
  def upcast(a: A, x: Any): a.L = x
  lazy val p: A { type L <: Nothing } = p
  val q = new A { type L = Any }
  def coerce2(x: Any): Int = upcast(p, x): p.L // error: not a legal path
}
