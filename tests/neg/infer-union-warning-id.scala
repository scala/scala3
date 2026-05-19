//> using options -Winfer-union -Wconf:id=E225:error

case class Pair[A](a: A, b: A)

val _ = Pair(1, "") // error