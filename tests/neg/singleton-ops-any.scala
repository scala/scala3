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

  val t01: ToString[1] = "1"
  val t02: ToString[-2L] = "-2"
  val t03: ToString[true] = "true"
  val t04: ToString[Int] = "Int" // error
  val t05: ToString[3.33] = "3.33"
  val t06: ToString["123"] = "123"

  val t40: IsConst[1] = true
  val t41: IsConst[2L] = true
  val t42: IsConst[-1.0] = true
  val t43: IsConst[false] = true
  val t44: IsConst["hi"] = true
  val t45: IsConst[Int] = false
  val one : Int = 1
  val t46 : IsConst[one.type] = false
  final val two = 2
  val t47 : IsConst[two.type] = true
  val t48: IsConst[Any] = true // error
  def isConst[X] : IsConst[X] = ???
  val t49 : true = isConst[1]
  val t50 : false = isConst[one.type]
  def isConst2[X <: Int, Y <: Int] : IsConst[X == Y] = ???
  val t51 : true = isConst2[1, 1]
  val t52 : false = isConst2[1, one.type]
  val t53 : true = isConst2[1, two.type]
}
