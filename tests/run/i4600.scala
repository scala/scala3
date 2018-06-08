import scala.language.dynamics

class ImplicitExample() extends Dynamic {
  def someMethod()(implicit s: String = "World"): String = s
  def applyDynamic(name: String)(args: Any*)(implicit s: String = "World"): String = name + s
}

class ImplicitTest {
  def t1() = {
    new ImplicitExample().someMethod()
  }

  def t2() = {
    implicit val s: String = "Hello"
    new ImplicitExample().someMethod()
  }

  def t3() = {
    new ImplicitExample().run()
  }

  def t4() = {
    implicit val s: String = "Hello"
    new ImplicitExample().run()
  }
}


object Test {
  def main(args: Array[String]) = {
    val it = new ImplicitTest
    assert(it.t1() == "World")
    assert(it.t2() == "Hello")
    assert(it.t3() == "runWorld")
    assert(it.t4() == "runHello")
  }
}
