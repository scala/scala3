class A1 {
  class A2(x: Int) extends AnyVal // error: value class may not be a member of another class
}
class B1 {
  def test = {
    class B2(x: Int) extends AnyVal // error: value class may not be a local class
  }
}
