import scala.util.boundary

object test:
  boundary[AnyRef^]:
    l1 ?=> // error // error
      boundary[Unit]: l2 ?=>
        boundary.break(l2)(using l1) // error
      ???
