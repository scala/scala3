//> using options -Wunused:imports
import language.experimental.captureChecking

package a:
  class T
  object M:
    def mk(): T = new T

package b:
  import a.T // warn: the result type of the method that HoistSuperArgs generates is not a use
  import a.M
  class B(x: Any)
  class C(n: Int) extends B({ def loc() = M.mk(); loc() })
