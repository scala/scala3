class B {
  import Macros._

  def f: Int = 3
  def g: A = foo // comment out `g` first, then enable `g`
}
