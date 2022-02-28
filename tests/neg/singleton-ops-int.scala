import scala.compiletime.ops.int.*

object Test {
  summon[2 + 3 =:= 6 - 1]
  summon[1763 =:= 41 * 43]
  summon[2 + 2 =:= 3] // error
  summon[29 * 31 =:= 900] // error
  summon[Int <:< Int + 1] // error
  summon[1 + Int <:< Int]

  val t0: 2 + 3 = 5
  final val two = 2
  final val three = 3
  val t0_b : two.type + three.type = 5
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

  val t32: Abs[0] = 0
  val t33: Abs[-1] = 1
  val t34: Abs[-1] = -1 // error
  val t35: Abs[1] = -1 // error

  val t36: Negate[-10] = 10
  val t37: Negate[10] = -10
  val t38: Negate[1] = 1 // error
  val t39: Negate[-1] = -1 // error

  val t40: Max[-1, 10] = 10
  val t41: Max[4, 2] = 4
  val t42: Max[2, 2] = 1 // error
  val t43: Max[-1, -1] = 0 // error

  val t44: Min[-1, 10] = -1
  val t45: Min[4, 2] = 2
  val t46: Min[2, 2] = 1 // error
  val t47: Min[-1, -1] = 0 // error

  val t48: ToString[213] = "213"
  val t49: ToString[-1] = "-1"

  val t52: 1 ^ 2 = 3
  val t53: 1 ^ 3 = 3 // error
  val t54: -1 ^ -2 = 1
  val t55: -1 ^ -3 = 1 // error

  val t56: BitwiseOr[1, 2] = 3
  val t57: BitwiseOr[10, 12] = 13 // error
  val t58: BitwiseOr[-11, 12] = -3
  val t59: BitwiseOr[-111, -10] = 0 // error

  val t60: BitwiseAnd[1, 1] = 1
  val t61: BitwiseAnd[1, 2] = 0
  val t62: BitwiseAnd[-1, -3] = 3 // error
  val t63: BitwiseAnd[-1, -1] = 1 // error

  val t64: 1 << 1 = 2
  val t65: 1 << 2 = 4
  val t66: 1 << 3 = 8
  val t67: 1 << 4 = 0 // error

  val t68: 100 >> 2 = 25
  val t69: 123456789 >> 71 = 964506
  val t70: -7 >> 3 = -1
  val t71: -7 >> 3 = 0 // error

  val t72: -1 >>> 10000 = 65535
  val t73: -7 >>> 3 = 536870911
  val t74: -7 >>> 3 = -1 // error

  val t75: NumberOfLeadingZeros[0] = 32
  val t76: NumberOfLeadingZeros[8] = 28
  val t77: NumberOfLeadingZeros[-1] = 0
  val t78: NumberOfLeadingZeros[-1] = 1 // error

  val t79: ToLong[1] = 1L
  val t80: ToLong[2] = 2 // error

  val t81: ToFloat[1] = 1.0f
  val t82: ToFloat[2] = 2 // error

  val t83: ToDouble[1] = 1.0
  val t84: ToDouble[2] = 2 // error

  // Singleton operations are covariant.
  type Plus2[X <: Int] = X + 2
  val x: Int = 5
  val xPlus2: Plus2[x.type] = (x + 2).asInstanceOf
  summon[xPlus2.type + 1 <:< Plus2[x.type] + 1]
  def mult(x: Int, y: Int): x.type * y.type = (x * y).asInstanceOf
  val y: x.type * x.type * x.type = mult(mult(x, x), x)
}
