//> using options -Werror -Wunused:privates

def f =
  new J:
    override val i = -1 // nowarn, trust override

def g =
  new J:
    override def i = -1 // nowarn, trust override

def h =
  new J:
    override def i() = -1 // nowarn correctly matches signature
