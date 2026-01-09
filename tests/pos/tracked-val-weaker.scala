import scala.language.experimental.modularity

object O:
  tracked val x = 1

def Test =
  tracked val x = 1
  val _: 1 = O.x
  val _: 1 = x
  tracked val y = x
  val _: x.type = y
  tracked val y1 = O.x
  val _: O.x.type = y1
