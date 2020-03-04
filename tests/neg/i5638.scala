class Opt{
  def isEmpty: Boolean = false
  def get[T]: Int = 1
}
object Extract{
  def unapply(x: Int): Opt = new Opt
}
object O {
  def m(x: Int) = x match {
    case Extract(r) =>  // error
      r  // error
  }
}
