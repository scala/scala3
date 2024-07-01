//> using options -coverage-exclude-packages:covtest.Klass

package covtest

class Klass {
  def abs(i: Int) =
    if i > 0 then
      i
    else
      -i
}

class Klass2 {
  def abs(i: Int) =
    if i > 0 then
      i
    else
      -i
}
