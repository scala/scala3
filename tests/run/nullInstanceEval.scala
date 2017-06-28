object Test extends App {
  val x = null
  assert(!x.isInstanceOf[String])
  assert(!(x: AnyRef).isInstanceOf[String])
}
