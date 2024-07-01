//> using options -coverage-exclude-files:.*ExcludeFile

package covtest

class Klass {
  def abs(i: Int) =
    if i > 0 then
      i
    else
      -i
}
