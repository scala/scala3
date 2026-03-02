import scala.util.boundary, boundary.Label
import runtime.suspend

trait Choice:
  def choose[A](choices: A*): A

// the handler
def choices[T](body: Choice ?=> T): Seq[T] =
  boundary[Seq[T]]:
    given Choice:
      def choose[A](choices: A*): A =
        suspend[A, Seq[T]](s => choices.flatMap(s.resume))
    Seq(body)

def choose[A](choices: A*)(using c: Choice): A = c.choose(choices*)

def TestChoices: Seq[Int] =
  choices:
    def x = choose(1, -2, -3)
    def y = choose("ab", "cde")
    val xx = x;
    xx + (
      if xx > 0 then
        val z = choose(xx / 2, xx * 2)
        y.length * z
      else y.length
    )

