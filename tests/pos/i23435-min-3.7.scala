//> using options -source:3.7

type Or[+A, +B] = A | B

val x: Or[Int, String] & Or[String, Int] = 3
val y: Or[Int & String, String & Int] = x // ok in 3.7, error in 3.8
val z: String = y
