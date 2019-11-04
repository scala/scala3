import scala.compiletime.testing.typeChecks

object Test extends App {

  inline final def assertCompiles(inline code: String): Boolean =
    if (typeChecks(code)) true else false

  inline val code = "1 + 1"
  val result = assertCompiles(code)
  assert(result)
}