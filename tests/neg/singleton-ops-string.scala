import scala.compiletime.ops.string.*

object Test {
  val t0: "Hello " + "world" = "Hello world"
  val t1: "" + "" = ""
  val t2: "3" + "" = "33" // error
  val t3: "Hello " + "world" = "error" // error
}
