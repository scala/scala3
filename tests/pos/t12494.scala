
trait Base {
  protected[Base] def f: Int
}
object Base {
  class Child extends Base {
    protected[Base] def f: Int = 42   // ok
    def test = f
  }
}
