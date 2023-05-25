import scala.util.boundary
import boundary.{break, Label}
import java.util.concurrent.TimeUnit

object loop:
  opaque type ExitLabel = Label[Unit]
  opaque type ContinueLabel = Label[Unit]

  inline def apply(inline op: (ExitLabel, ContinueLabel) ?=> Unit): Unit =
    boundary { exitLabel ?=>
      while true do
        boundary { continueLabel ?=>
          op(using exitLabel, continueLabel)
        }
    }
  end apply

  inline def exit()(using ExitLabel): Unit =
    break()

  inline def continue()(using ContinueLabel): Unit =
    break()
end loop

def testLoop(xs: List[Int]) =
  var current = xs
  var sum = 0
  loop:
    // This should be convertible to labeled returns but isn't, since
    // the following code is still passed as a closure to `boundary`.
    // That's probably due to the additional facade operations necessary
    // for opaque types.
    if current.isEmpty then loop.exit()
    val hd = current.head
    current = current.tail
    if hd == 0 then loop.exit()
    if hd < 0 then loop.continue()
    sum += hd
  sum

@main def Test =
  assert(testLoop(List(1, 2, 3, -2, 4, -3, 0, 1, 2, 3)) == 10)
  assert(testLoop(List()) == 0)
  assert(testLoop(List(-2, -3, 0, 1)) == 0)


