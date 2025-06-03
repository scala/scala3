
class A:
  class B:
    class C:
      def foo(): Int = m

  def bar(b: B) = new b.C().foo()

  bar(new B)  // warn

  val m = 10
