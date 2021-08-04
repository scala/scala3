// This is crazy:
type Get0 = OK[Int, Unit]
def get0: Handler[Get0] = IO[Unit]()

case class HandlerAlt[A](value: Handler[A])

type Handler[API] = handler.Go[API]

case class IO[A]()
case class OK[A, B]()

object handler:
  // Starter for Handler reduction:
  type Go[API] = API match
    case _ =>
      HandlerSingle[API]

  type HandlerSingle[X] = X match
    case OK[_, response] =>
      IO[response]

object Minimized {
  case class HandlerAlt[A](value: M2[A])

  type M1[X] = X match {
    case _ => M2[X]
  }

  type M2[X] = X match {
    case Int => String
  }
}
