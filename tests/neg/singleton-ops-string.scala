import scala.compiletime.ops.string.*

object Test {
  val t0: "Hello " + "world" = "Hello world"
  val t1: "" + "" = ""
  val t2: "3" + "" = "33" // error
  val t3: "Hello " + "world" = "error" // error

  val t4: Length["Hello"] = 5
  val t5: Length[""] = 0
  val t6: Length["1"] = 7 // error

  val t7: Substring["hamburger", 4, 8] = "urge"
  val t8: Substring["hamburger", 4, 8] = "urger" // error

  val t9: Matches["hamburger", "ham.*"] = true
  val t10: Matches["hamburger", "ham.*"] = false // error
}
