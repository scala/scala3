object Test extends App {
  val y: Int => Int = null
  var x = y: Int => Int
  assert(x == null)
}