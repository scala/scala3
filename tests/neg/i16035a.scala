trait S:
  type Uses[A, B] <: A ?=> B
  object Uses:
    def apply[A, B](fn: A ?=> B): Uses[A, B] = fn // error
  val uses1 =
    given Int = 1
    Uses[Int, String](i ?=> s"*$i*")

object I extends S:
  type Uses[A, B] = A ?=> B
  val uses2 =
    given Int = 1
    Uses[Int, String](i ?=> s"*$i*")

