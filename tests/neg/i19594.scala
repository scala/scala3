import scala.annotation.implicitNotFound

@implicitNotFound("Can you see me?!")
trait Compare[A, B]

object example extends App:

  // The presence of the below default argument prevents the `implicitNotFound` message from appearing
  def assertEquals[A, B](a: A, b: B, clue: => Any = "values are not the same")
                        (implicit comp: Compare[A, B]): Unit = ()

  assertEquals(true, 1, "values are not the same") // error
  assertEquals(true, 1) // error
