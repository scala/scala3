object Outer {
  def foo = {
    trait Mixin
    class E extends Mixin
    object SomeObject {
      val A = E()
    }
  }
}
