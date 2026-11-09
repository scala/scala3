//> using options -Wunused:imports

object A:
  extension (a: Int) def f: Int = a

object B:
  import A.f as f2
  val _ = 2.f2

object C:
  import A.f as f3
  val _ = f3(2)

object D:
  import A.f as f4 // warn
  val _ = 2
