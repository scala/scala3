//> using options -coverage-exclude-packages:covtest

package covtest

class Klass {
  def abs(i: Int) =
    if i > 0 then
      i
    else
      -i
}
