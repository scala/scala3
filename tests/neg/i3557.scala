
class C1 {
  @annotation.varargs
  def f(values: String*) = ()
}
class C2 {
  @scala.annotation.varargs
  def f(values: String*) = ()
  def g: String => Int = s => hashCode
  class C3 {
    @f.varargs                    // error
    def f(values: String) = ()
  }
}
