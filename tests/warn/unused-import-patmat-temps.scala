//> using options -Wunused:imports

package a:
  class T
  object M:
    def mk(): T = new T
    def pair(): (T, Int) = (new T, 1)

package b:
  import a.T // warn: the types of the definitions that PatternMatcher generates are not uses
  import a.M
  def f = M.mk() match
    case t => t.toString
  def g = M.pair() match
    case (t, n) => t.toString + n
