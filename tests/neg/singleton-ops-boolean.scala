import scala.compiletime.ops.boolean.*

object Test {
  val t0: ![true] = false
  val t1: ![false] = true
  val t2: ![true] = true // error
  val t3: ![false] = false // error

  val t4: true && true = true
  val t5: true && false = false
  val t6: false && true = true // error
  val t7: false && false = true // error

  val t8: true ^ true = false
  val t9: false ^ true = true
  val t10: false ^ false = true // error
  val t11: true ^ false = false // error

  val t12: true || true = true
  val t13: true || false = true
  val t14 false || true = false // error
  val t15: false || false = true // error
}
