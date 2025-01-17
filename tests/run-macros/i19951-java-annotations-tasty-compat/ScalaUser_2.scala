class ScalaUser {

  @JavaAnnot(a = 5)
  def f2(): Int = 1

  @JavaAnnot(a = 5, b = "foo")
  def f3(): Int = 1

  @JavaAnnot(a = 5, b = "foo", c = 3)
  def f4(): Int = 1

  @JavaAnnot(a = 5, c = 3)
  def f5(): Int = 1

  @JavaAnnot(a = 5, c = 3, b = "foo")
  def f6(): Int = 1

  @JavaAnnot(b = "foo", c = 3, a = 5)
  def f7(): Int = 1

  @JavaAnnot(b = "foo", a = 5)
  def f8(): Int = 1
}
