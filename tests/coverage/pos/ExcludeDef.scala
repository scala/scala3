//> using options -coverage-exclude-packages:covtest\..*

package covtest

def abs(i: Int) =
  if i > 0 then
    i
  else
    -i
