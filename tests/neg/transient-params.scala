class Context

class Test1()(implicit @transient ctx: Context) { // error
  def test1 = implicitly[Context]
}

class Test2()(@transient ctx: Context) { // error
  def test1 = ctx
}

class Test3()(implicit @transient ctx: Context) { // OK
  val foo = implicitly[Context]
}

final class Test4(@transient val obj: Any): // OK
  def foo = obj

final class Test5(@transient private val obj: Any): // OK
  def foo = obj

final class Test6(@transient private[this] val obj: Any): // OK
  def foo = obj

final class Test7(@transient obj: Any): // error
  def foo = obj










