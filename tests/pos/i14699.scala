def always(block: => Unit): Unit = {}
def always(args: Int*)(block: => Unit): Unit ={}

def test =
  val x = always{}
  val xc: Unit = x
  always(1,2,3) {}
