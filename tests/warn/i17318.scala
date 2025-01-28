
//> using options -Werror -Wunused:all

object events {
  final val PollOut = 0x002
  transparent inline def POLLIN = 0x001
}

def withShort(v: Short): Unit = ???
def withInt(v: Int): Unit = ???

def usage() =
  import events.POLLIN // reports unused
  def v: Short = POLLIN
  println(v)

def usage2() =
  import events.POLLIN // reports unused
  withShort(POLLIN)

def usage3() =
  import events.POLLIN // does not report unused
  withInt(POLLIN)

def usage4() =
  import events.POLLIN // reports unused
  withShort(POLLIN)

def value = 42
def withDouble(v: Double): Unit = ???
def usage5() = withDouble(value)
def usage6() = withShort(events.POLLIN)
def usage7() = withShort(events.PollOut)
