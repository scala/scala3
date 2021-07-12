object A:

  opaque infix type over[A, B] = (A, B)
  def f: over[Int, String] = (1, "")

object B:
  import A.*
  val x: over[String, Int] = f // error
