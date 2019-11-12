val t0: Int = 0
val t1: scala.compiletime.S[-1] = 0 // error
val t2: scala.compiletime.S[Int] = 1
val t3: scala.compiletime.S[Int] = 0 // error
val t4: scala.compiletime.S[Int] = t0 // error

val t5: scala.compiletime.S[2147483647] = -2147483648 // error
val t6: scala.compiletime.S[-2147483648] = -2147483647 // error
