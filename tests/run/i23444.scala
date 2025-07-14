
import annotation.*

class Path(action: () => Unit, parent: Option[Path]):
  object O:
    @tailrec
    def apply(): Unit =
      action()

      parent match
        case Some(p) =>
          p.O.apply()
        case None =>

@main def Test: Unit =
  var counter = 0
  val fun = () => {
    counter += 1
    if counter > 2 then throw AssertionError("bad loop")
  }
  val path = Path(fun, Some(Path(fun, None)))
  path.O()
