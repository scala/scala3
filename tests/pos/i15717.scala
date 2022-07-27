// scalac: -Werror
class Test:
  def pmat(xs: java.util.Vector[_]): String = xs.get(0) match
    case d: Double => d.toString() // was: error: unreachable case, which is spurious
    case _         => "shrug"

  def pmatR(xs: java.util.Vector[_]): String =
    val scr = xs.get(0)
    1.0 match
      case `scr` => scr.toString() // for the reverse provablyDisjoint case
      case _     => "shrug"

  def test =
    val x = new java.util.Vector[Double]()
    x.add(1.0)
    pmat(x)
