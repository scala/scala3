//> using options -source:future

type Or[+A, +B] = A | B

val x: Or[Int, String] & Or[String, Int] = 3
val y: Or[Int & String, String & Int] = x // error
val z: String = y
