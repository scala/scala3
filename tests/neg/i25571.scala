//> using options -Werror -Wunused:nowarn

class A {
  @annotation.nowarn("msg=aaaaa")
  def f: Int = 2
}
// nopos-error
