import language.experimental.captureChecking

import scala.util.boundary

object test:
  boundary[boundary.Label[Unit]]: l1 ?=> // error
    boundary[Unit]: l2 ?=>
      boundary.break(l2)(using l1) // error
    ???
