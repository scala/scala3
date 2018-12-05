class B {
  def getA(implicit a: A): A = a
  def test = {
    getA
  }
}
