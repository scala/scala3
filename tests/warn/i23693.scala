//> using options -Wtostring-interpolated

// verify warning messages and runtime result

case class K(i: Int):
  def show: Unit = ()

@main def Test =
  val k = K(42)
  println:
    s"k == $k" // warn
  println:
    raw"\k == \$k" // warn
  println:
    f"k == $k" // warn
  println:
    f"k == $k%s" // warn
  println:
    s"show == ${k.show}" // warn
