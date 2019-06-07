object Test extends App {

  class TC

  implied StringListOps given TC {
    type T = List[String]
    def (x: T) foo (y: T) = (x ++ y, the[TC])
    def (x: T) bar (y: Int) = (x(0)(y), the[TC])
  }

  def test given TC = {
    assert(List("abc").foo(List("def"))._1 == List("abc", "def"))
    assert(List("abc").bar(2)._1 == 'c')
  }

  test given TC()
}