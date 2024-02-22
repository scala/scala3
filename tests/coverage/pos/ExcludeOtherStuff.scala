//> using options -coverage-exclude-classlikes:covtest.Oject,covtest.Tait

package covtest

object Oject {
  def abs(i: Int) =
    if i > 0 then
      i
    else
      -i
}

trait Tait {
  def abs(i: Int): Int
}
