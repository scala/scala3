object bounds_specialization {
  /*class Foo[@specialized K] {
    def bar[@specialized U](u: U) {
      def dough[@specialized V](v: V) {
        println("innerMethod")
      }
      dough(1.toShort)
      dough('c')
    }
    bar(2.toShort)
    bar('d')
  }
*/
  def kung[@specialized(Int, Double) T <: AnyRef](t: T): T = {
    t
  }

  def fu[@specialized(Int, Double) T >: Nothing](t: T): T = {
    t
  }
}