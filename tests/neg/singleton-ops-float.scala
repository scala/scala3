import scala.compiletime.ops.float.*

object Test {
  summon[2.0f + 3.0f =:= 6.0f - 1.0f]
  summon[1763.0f =:= 41.0f * 43.0f]
  summon[2.0f + 2.0f =:= 3.0f] // error
  summon[29.0f * 31.0f =:= 900.0f] // error
  summon[Float <:< Float + 1.0f] // error
  summon[1.0f + Float <:< Float]

  val t0: 2.0f + 3.0f = 5.0f
  val t1: 2.0f + 2.0f = 5.0f // error
  val t2: -1.0f + 1.0f = 0.0f
  val t3: -5.0f + -5.0f = -11.0f // error

  val t4: 10.0f * 20.0f = 200.0f
  val t5: 30.0f * 10.0f = 400.0f // error
  val t6: -10.0f * 2.0f  = -20.0f
  val t7: -2.0f * -2.0f = 4.0f

  val t8: 10.0f / 2.0f = 5.0f
  val t9: 11.0f / -2.0f = -5.5f
  val t10: 2.0f / 4.0f = 2.0f // error

  val t12: 10.0f % 3.0f = 1.0f
  val t13: 12.0f % 2.0f = 1.0f // error
  val t14: 1.0f % -3.0f = 1.0f

  val t16: 1.0f < 0.0f = false
  val t17: 0.0f < 1.0f = true
  val t18: 10.0f < 5.0f = true // error
  val t19: 5.0f < 10.0f = false // error

  val t20: 1.0f <= 0.0f = false
  val t21: 1.0f <= 1.0f = true
  val t22: 10.0f <= 5.0f = true // error
  val t23: 5.0f <= 10.0f = false // error

  val t24: 1.0f > 0.0f = true
  val t25: 0.0f > 1.0f = false
  val t26: 10.0f > 5.0f = false // error
  val t27: 5.0f > 10.0f = true // error

  val t28: 1.0f >= 1.0f = true
  val t29: 0.0f >= 1.0f = false
  val t30: 10.0f >= 5.0f = false // error
  val t31: 5.0f >= 10.0f = true // error

  val t32: Abs[0.0f] = 0.0f
  val t33: Abs[-1.0f] = 1.0f
  val t34: Abs[-1.0f] = -1.0f // error
  val t35: Abs[1.0f] = -1.0f // error

  val t36: Negate[-10.0f] = 10.0f
  val t37: Negate[10.0f] = -10.0f
  val t38: Negate[1.0f] = 1.0f // error
  val t39: Negate[-1.0f] = -1.0f // error

  val t40: Max[-1.0f, 10.0f] = 10.0f
  val t41: Max[4.0f, 2.0f] = 4.0f
  val t42: Max[2.0f, 2.0f] = 1.0f // error
  val t43: Max[-1.0f, -1.0f] = 0.0f // error

  val t44: Min[-1.0f, 10.0f] = -1.0f
  val t45: Min[4.0f, 2.0f] = 2.0f
  val t46: Min[2.0f, 2.0f] = 1.0f // error
  val t47: Min[-1.0f, -1.0f] = 0.0f // error

  val t79: ToInt[1.0f] = 1
  val t80: ToInt[3.0f] = 2 // error

  val t81: ToLong[1.0f] = 1L
  val t82: ToLong[2.0f] = 2 // error

  val t83: ToDouble[1.0f] = 1.0
  val t84: ToDouble[2.0f] = 2 // error
}
