package genericouterinnermember

class Outer[A]:
  class Inner:
    def use(a: A): A = a
  @annotation.targetName("InnerX")
  class Inner2:
    def use(a: A): A = a
  def inner: Inner = new Inner
  def inner2: Inner2 = new Inner2
