import language.experimental.safe
def assertPure(op: () -> Unit): Unit = ()
def test1(): Unit =
  val ref = new scala.runtime.LongRef(0L)  // error
  assertPure: () =>
    ref.elem = 42  // error
def test2(): Unit =
  val ref = new scala.runtime.ObjectRef("")  // error
  assertPure: () =>
    ref.elem = "sk-123456"  // error
def test3(): Unit =
  val ref = new scala.runtime.VolatileIntRef(0)  // error
  assertPure: () =>
    ref.elem = 42  // error
def test4(): Unit =
  val ref = new scala.runtime.VolatileObjectRef[String]("")  // error
  assertPure: () =>
    ref.elem = "secret"  // error
def test5(): Unit =
  val ref = new scala.runtime.LazyInt  // error
  assertPure: () =>
    ref.initialize(42)  // error
def test6(): Unit =
  val ref = new scala.runtime.LazyRef[String]  // error
  assertPure: () =>
    ref.initialize("hello")  // error
