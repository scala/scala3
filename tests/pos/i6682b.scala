object O{
  val v = [T] => (y:T) => (x:y.type) => 3
  val u = [T] => (y:T) => (x:y.type) => 3
  def m = v(u)
}
