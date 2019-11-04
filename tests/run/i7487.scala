import scala.compiletime.testing.typeChecks

object Test extends App {
  inline val code = "1 + 1"
  val result1: Boolean = typeChecks(code)    // true
  val result2: Boolean = typeChecks("1 + 1") // true
  val result3: Boolean = typeChecks("1" + "1") // true
  assert(result1)
  assert(result2)
  assert(result3)
}
