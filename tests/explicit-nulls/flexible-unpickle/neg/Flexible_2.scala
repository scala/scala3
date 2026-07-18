import unsafe.*

@A[String] class C

def test =

  val ii = Bar.f[Int](null) // error
  val jj = Bar.g[String](null) // error
  val jj2 = Bar.g[String | Null](null) // ok
  val kk = Bar.g2[String](null) // error
  val kk2 = Bar.g2[String | Null](null) // ok

  val bar_x: Int = Bar.x
  val bar_y: String | Null = Bar.y.replaceAll(" ","")

  def testUsingFoo(using Foo[Option]) = Bar.h(null)

  val ii2 = Bar2[String]().f(null) // error
  val ii3 = Bar2[String | Null]().f(null) // ok

  val a = Bar.ff(
    (x: AnyRef) => x.toString,
    42
  )