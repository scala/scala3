import annotation.constructorOnly
class Context

class Test1()(implicit @constructorOnly ctx: Context) { // error
  def test1 = implicitly[Context]
}

class Test2()(@constructorOnly ctx: Context) { // error
  def test1 = ctx
}

class Test3()(implicit @constructorOnly ctx: Context) { // OK
  val foo = implicitly[Context]
}
