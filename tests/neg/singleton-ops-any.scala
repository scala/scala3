import scala.compiletime.ops.any.*

object Test {
  val t32: 1 == 1 = true
  val t33: 0 == false = false
  val t34: 10 == "5" = true // error
  val t35: 10 == 10 = false // error
  val t35string: "10" == "10" = true

  val t36: 1 != 1 = false
  val t37: 0 != 1 = true
  val t38: false != 5 = false // error
  val t39: 10 != 10 = true // error
}
