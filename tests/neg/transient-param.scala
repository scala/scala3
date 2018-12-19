import annotation.transientParam
class Context

class Test1()(implicit @transientParam ctx: Context) { // error
  def test1 = implicitly[Context]
}

class Test2()(@transientParam ctx: Context) { // error
  def test1 = ctx
}

class Test3()(implicit @transientParam ctx: Context) { // OK
  val foo = implicitly[Context]
}
