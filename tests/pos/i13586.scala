class Foo:
 inline def test(): Unit = this

class Bar extends Foo:
  def test(s: String) = super.test()
