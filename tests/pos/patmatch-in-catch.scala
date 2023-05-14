sealed abstract class Action
case object MainReturn extends Action
case object ExceptionNormalExit extends Action
case class CaughtException(action: Action) extends RuntimeException

def driver(action: Action): Action =
    val result =
      try action
      catch case CaughtException(action) =>
        action match
          case ExceptionNormalExit =>
            action
          case _ =>
            ???
    result