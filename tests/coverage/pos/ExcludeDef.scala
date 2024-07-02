//> using options -coverage-exclude-classlikes:covtest\..*

package covtest

def abs(i: Int) =
  if i > 0 then
    i
  else
    -i
