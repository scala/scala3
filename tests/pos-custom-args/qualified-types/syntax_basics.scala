abstract class BoolExprs:
  val c: Boolean
  def f(b: Boolean): Boolean
  val v01: {b: Boolean with true}
  val v02: {b: Boolean with false}
  val v03: {b: Boolean with !b}
  val v04: {b: Boolean with b && b}
  val v05: {b: Boolean with b || b}
  val v06: {b: Boolean with c}
  val v07: {b: Boolean with !c}
  val v08: {b: Boolean with b && c}
  val v09: {b: Boolean with b || c}
  val v10: {b: Boolean with f(b)}
  val w01 = v01
  val w02 = v02
  val w03 = v03
  val w04 = v04
  val w05 = v05
  val w06 = v06
  val w07 = v07
  val w08 = v08
  val w09 = v09
  val w10 = v10

abstract class IntExprs:
  val c: Int
  def f(n: Int): Int
  val v01: {n: Int with n == 0}
  val v02: {n: Int with -n == 0}
  val v03: {n: Int with n != 0}
  val v04: {n: Int with n > 0}
  val v05: {n: Int with n >= 0}
  val v06: {n: Int with n < 0}
  val v07: {n: Int with n <= 0}
  val v08: {n: Int with n == c}
  val v09: {n: Int with n != c}
  val v10: {n: Int with n > c}
  val v11: {n: Int with n >= c}
  val v12: {n: Int with n < c}
  val v13: {n: Int with n <= c}
  val v14: {n: Int with n == f(n)}
  val w01 = v01
  val w02 = v02
  val w03 = v03
  val w04 = v04
  val w05 = v05
  val w06 = v06
  val w07 = v07
  val w08 = v08
  val w09 = v09
  val w10 = v10
  val w11 = v11
  val w12 = v12
  val w13 = v13
  val w14 = v14
