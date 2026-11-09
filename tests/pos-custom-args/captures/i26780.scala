import language.experimental.captureChecking

class A(x: AnyRef^)

def ok =
  val a = classOf[A]
  val c: Class[A] = a

def test1 =
  val a = classOf[A]
  val b = identity(a)
  val c: Class[A] = a // was error: Found (a : Class[A{val x: Object^{any}}^{}]^{})

def test2 =
  val a = classOf[A]
  val b = identity(a)
  val c: Class[A] = b // was error: Found (b : Class[A{val x: Object^{any}}^{}]^{})

class B(val x: AnyRef^)

def test3 =
  val a = classOf[B]
  val b: B = ???
  val d = b.x       // any other member lookup of `x` in between also triggered the error
  val c: Class[B] = a
