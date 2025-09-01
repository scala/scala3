class dummy(b: Any) extends annotation.StaticAnnotation

class X:
  def foo() = 1
  def bar() = 2
  def eq(x: X) = true
  def id(): this.type = this

class Y extends X:
  override def bar() = 2
  override def eq(x: X) = true

def f(x: Int) = x
def g(x: String) = x
def g(x: Int) = x

object AnnotationTests:
  def foo1(elem: Int, bla: Int @dummy(Array(elem))) = bla
  def foo2(elem: X, bla: Int @dummy(elem.foo())) = bla
  def foo3(elem: Y, bla: Int @dummy(elem.foo())) = bla
  def foo4(elem: X, bla: Int @dummy(elem.bar())) = bla
  def foo5(elem: Y, bla: Int @dummy(elem.bar())) = bla
  def foo6(elem: X, bla: Int @dummy(elem.eq(X()))) = bla
  def foo7(elem: Y, bla: Int @dummy(elem.eq(Y()))) = bla
  def foo8(elem: X, bla: Int @dummy(elem.id().foo())) = bla
  def foo9(elem: Y, bla: Int @dummy(elem.id().foo())) = bla
  def foo10(elem: Int, bla: Int @dummy(f(elem))) = bla
  def foo11(elem: Int, bla: Int @dummy(g(elem))) = bla
  def foo12(elem: Int, bla: Int @dummy(0 == elem)) = bla
  def foo13(elem: Int, bla: Int @dummy(elem == 0)) = bla
