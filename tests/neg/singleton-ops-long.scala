import scala.compiletime.ops.long.*

object Test {
  summon[2L + 3L =:= 6L - 1L]
  summon[1763L =:= 41L * 43L]
  summon[2L + 2L =:= 3L] // error
  summon[29L * 31L =:= 900L] // error
  summon[Long <:< Long + 1L] // error
  summon[1L + Long <:< Long]

  val t0: 2L + 3L = 5L
  val t1: 2L + 2L = 5L // error
  val t2: -1L + 1L = 0L
  val t3: -5L + -5L = -11L // error

  val t4: 10L * 20L = 200L
  val t5: 30L * 10L = 400L // error
  val t6: -10L * 2L  = -20L
  val t7: -2L * -2L = 4L

  val t8: 10L / 2L = 5L
  val t9: 11L / -2L = -5L // Integer division
  val t10: 2L / 4L = 2L // error
  val t11: -1L / 0L = 1L // error

  val t12: 10L % 3L = 1L
  val t13: 12L % 2L = 1L // error
  val t14: 1L % -3L = 1L
  val t15: -3L % 0L = 0L // error

  val t16: 1L < 0L = false
  val t17: 0L < 1L = true
  val t18: 10L < 5L = true // error
  val t19: 5L < 10L = false // error

  val t20: 1L <= 0L = false
  val t21: 1L <= 1L = true
  val t22: 10L <= 5L = true // error
  val t23: 5L <= 10L = false // error

  val t24: 1L > 0L = true
  val t25: 0L > 1L = false
  val t26: 10L > 5L = false // error
  val t27: 5L > 10L = true // error

  val t28: 1L >= 1L = true
  val t29: 0L >= 1L = false
  val t30: 10L >= 5L = false // error
  val t31: 5L >= 10L = true // error

  val t32: Abs[0L] = 0L
  val t33: Abs[-1L] = 1L
  val t34: Abs[-1L] = -1L // error
  val t35: Abs[1L] = -1L // error

  val t36: Negate[-10L] = 10L
  val t37: Negate[10L] = -10L
  val t38: Negate[1L] = 1L // error
  val t39: Negate[-1L] = -1L // error

  val t40: Max[-1L, 10L] = 10L
  val t41: Max[4L, 2L] = 4L
  val t42: Max[2L, 2L] = 1L // error
  val t43: Max[-1L, -1L] = 0L // error

  val t44: Min[-1L, 10L] = -1L
  val t45: Min[4L, 2L] = 2L
  val t46: Min[2L, 2L] = 1L // error
  val t47: Min[-1L, -1L] = 0L // error

  val t52: 1L ^ 2L = 3L
  val t53: 1L ^ 3L = 3L // error
  val t54: -1L ^ -2L = 1L
  val t55: -1L ^ -3L = 1L // error

  val t56: BitwiseOr[1L, 2L] = 3L
  val t57: BitwiseOr[10L, 12L] = 13L // error
  val t58: BitwiseOr[-11L, 12L] = -3L
  val t59: BitwiseOr[-111L, -10L] = 0L // error

  val t60: BitwiseAnd[1L, 1L] = 1L
  val t61: BitwiseAnd[1L, 2L] = 0L
  val t62: BitwiseAnd[-1L, -3L] = 3L // error
  val t63: BitwiseAnd[-1L, -1L] = 1L // error

  val t64: 1L << 1L = 2L
  val t65: 1L << 2L = 4L
  val t66: 1L << 3L = 8L
  val t67: 1L << 4L = 0L // error

  val t68: 100L >> 2L = 25L
  val t69: 123456789L >> 71L = 964506L
  val t70: -7L >> 3L = -1L
  val t71: -7L >> 3L = 0L // error

  val t72: -1L >>> 10000L = 281474976710655L
  val t73: -7L >>> 3L = 2305843009213693951L
  val t74: -7L >>> 3L = -1L // error

  val t75: NumberOfLeadingZeros[0L] = 64
  val t76: NumberOfLeadingZeros[8L] = 60
  val t77: NumberOfLeadingZeros[-1L] = 0
  val t78: NumberOfLeadingZeros[-1L] = 1 // error

  val t79: ToInt[1L] = 1
  val t80: ToInt[3L] = 2 // error

  val t81: ToFloat[1L] = 1.0f
  val t82: ToFloat[2L] = 2 // error

  val t83: ToDouble[1L] = 1.0
  val t84: ToDouble[2L] = 2 // error
}
