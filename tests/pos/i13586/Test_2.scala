class Foo {
 inline def test(): Unit = {
   ${ Position.withPosition[Unit]('{ _ => this }) }
 }
}

class Bar extends Foo {
  def test(s: String) =
    super.test()
}
