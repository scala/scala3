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

  val t11: CharAt["String", 0] = 'S'
  val t12: CharAt["String", 1] = 't'
  val t13: CharAt["String", 2] = '!' // error
  //                             ^^^
  //                             Found:    ('!' : Char)
  //                             Required: ('r' : Char)
  val t14: CharAt["String", 3] = '!' // error
  //                             ^^^
  //                             Found:    ('!' : Char)
  //                             Required: ('i' : Char)
  val t15: CharAt["String", 4] = 'n'
  val t16: CharAt["String", 5] = 'g'
  val t17: CharAt["String", 6]  = '!' // error
  //       ^
  //       String index out of range: 6
  val t18: CharAt["String", -1] = '?' // error
  //       ^
  //       String index out of range: -1
}
