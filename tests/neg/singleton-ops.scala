import scala.compiletime.ops._

object Test {
  summon[2 + 3 =:= 6 - 1]
  summon[1763 =:= 41 * 43]
  summon[2 + 2 =:= 3] // error
  summon[29 * 31 =:= 900] // error
  summon[Int <:< Int + 1] // error
  summon[1 + Int <:< Int]

  val t0: 2 + 3 = 5
  val t1: 2 + 2 = 5 // error
  val t2: -1 + 1 = 0
  val t3: -5 + -5 = -11 // error

  val t4: 10 * 20 = 200
  val t5: 30 * 10 = 400 // error
  val t6: -10 * 2  = -20
  val t7: -2 * -2 = 4

  val t8: 10 / 2 = 5
  val t9: 11 / -2 = -5 // Integer division
  val t10: 2 / 4 = 2 // error
  val t11: -1 / 0 = 1 // error

  val t12: 10 % 3 = 1
  val t13: 12 % 2 = 1 // error
  val t14: 1 % -3 = 1
  val t15: -3 % 0 = 0 // error

  val t16: 1 < 0 = false
  val t17: 0 < 1 = true
  val t18: 10 < 5 = true // error
  val t19: 5 < 10 = false // error

  val t20: 1 <= 0 = false
  val t21: 1 <= 1 = true
  val t22: 10 <= 5 = true // error
  val t23: 5 <= 10 = false // error

  val t24: 1 > 0 = true
  val t25: 0 > 1 = false
  val t26: 10 > 5 = false // error
  val t27: 5 > 10 = true // error

  val t28: 1 >= 1 = true
  val t29: 0 >= 1 = false
  val t30: 10 >= 5 = false // error
  val t31: 5 >= 10 = true // error

  val t32: 1 == 1 = true
  val t33: 0 == 1 = false
  val t34: 10 == 5 = true // error
  val t35: 10 == 10 = false // error

  val t36: 1 != 1 = false
  val t37: 0 != 1 = true
  val t38: 10 != 5 = false // error
  val t39: 10 != 10 = true // error

  val t40: Abs[0] = 0
  val t41: Abs[-1] = 1
  val t42: Abs[-1] = -1 // error
  val t43: Abs[1] = -1 // error

  val t44: Negate[-10] = 10
  val t45: Negate[10] = -10
  val t46: Negate[1] = 1 // error
  val t47: Negate[-1] = -1 // error

  val t48: Max[-1, 10] = 10
  val t49: Max[4, 2] = 4
  val t50: Max[2, 2] = 1 // error
  val t51: Max[-1, -1] = 0 // error

  val t52: Min[-1, 10] = -1
  val t53: Min[4, 2] = 2
  val t54: Min[2, 2] = 1 // error
  val t55: Min[-1, -1] = 0 // error

  val t56: true && true = true
  val t57: true && false = false
  val t58: false && true = true // error
  val t59: false && false = true // error

  val t60: true || true = true
  val t61: true || false = true
  val t62: false || true = false // error
  val t63: false || false = true // error

  val t64: ![true] = false
  val t65: ![false] = true
  val t66: ![true] = true // error
  val t67: ![false] = false // error
}
