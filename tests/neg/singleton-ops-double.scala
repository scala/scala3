import scala.compiletime.ops.double.*

object Test {
  summon[2.0 + 3.0 =:= 6.0 - 1.0]
  summon[1763.0 =:= 41.0 * 43.0]
  summon[2.0 + 2.0 =:= 3.0] // error
  summon[29.0 * 31.0 =:= 900.0] // error
  summon[Double <:< Double + 1.0] // error
  summon[1.0 + Double <:< Double]

  val t0: 2.0 + 3.0 = 5.0
  val t1: 2.0 + 2.0 = 5.0 // error
  val t2: -1.0 + 1.0 = 0.0
  val t3: -5.0 + -5.0 = -11.0 // error

  val t4: 10.0 * 20.0 = 200.0
  val t5: 30.0 * 10.0 = 400.0 // error
  val t6: -10.0 * 2.0  = -20.0
  val t7: -2.0 * -2.0 = 4.0

  val t8: 10.0 / 2.0 = 5.0
  val t9: 11.0 / -2.0 = -5.5
  val t10: 2.0 / 4.0 = 2.0 // error

  val t12: 10.0 % 3.0 = 1.0
  val t13: 12.0 % 2.0 = 1.0 // error
  val t14: 1.0 % -3.0 = 1.0

  val t16: 1.0 < 0.0 = false
  val t17: 0.0 < 1.0 = true
  val t18: 10.0 < 5.0 = true // error
  val t19: 5.0 < 10.0 = false // error

  val t20: 1.0 <= 0.0 = false
  val t21: 1.0 <= 1.0 = true
  val t22: 10.0 <= 5.0 = true // error
  val t23: 5.0 <= 10.0 = false // error

  val t24: 1.0 > 0.0 = true
  val t25: 0.0 > 1.0 = false
  val t26: 10.0 > 5.0 = false // error
  val t27: 5.0 > 10.0 = true // error

  val t28: 1.0 >= 1.0 = true
  val t29: 0.0 >= 1.0 = false
  val t30: 10.0 >= 5.0 = false // error
  val t31: 5.0 >= 10.0 = true // error

  val t32: Abs[0.0] = 0.0
  val t33: Abs[-1.0] = 1.0
  val t34: Abs[-1.0] = -1.0 // error
  val t35: Abs[1.0] = -1.0 // error

  val t36: Negate[-10.0] = 10.0
  val t37: Negate[10.0] = -10.0
  val t38: Negate[1.0] = 1.0 // error
  val t39: Negate[-1.0] = -1.0 // error

  val t40: Max[-1.0, 10.0] = 10.0
  val t41: Max[4.0, 2.0] = 4.0
  val t42: Max[2.0, 2.0] = 1.0 // error
  val t43: Max[-1.0, -1.0] = 0.0 // error

  val t44: Min[-1.0, 10.0] = -1.0
  val t45: Min[4.0, 2.0] = 2.0
  val t46: Min[2.0, 2.0] = 1.0 // error
  val t47: Min[-1.0, -1.0] = 0.0 // error

  val t79: ToInt[1.0] = 1
  val t80: ToInt[3.0] = 2 // error

  val t81: ToLong[1.0] = 1L
  val t82: ToLong[2.0] = 2 // error

  val t83: ToFloat[1.0] = 1.0f
  val t84: ToFloat[2.0] = 2 // error
}
