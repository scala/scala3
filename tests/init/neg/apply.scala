case class A(b: B)

object A:
  def foo(b: B) = new A(b)
  inline def bar(b: B) = new A(b)

class B:
  val a = A(this)
  val a2 = A.foo(this)   // error
  val a3 = A.bar(this)

// test receiver is ThisRef

object O:
  case class A(b: B)

  object A:
    def foo(b: B) = new A(b)
    inline def bar(b: B) = new A(b)

  class B:
    val a = A(this)
    val a2 = A.foo(this)   // error
    val a3 = A.bar(this)

  val b = new B
end O


// test receiver is Warm

class M(n: N):
  case class A(b: B)

  object A:
    def foo(b: B) = new A(b)
    inline def bar(b: B) = new A(b)

  class B:
    val a = A(this)
    val a2 = A.foo(this)   // error
    val a3 = A.bar(this)
end M

class N:
  val m = new M(this)
  val b = new m.B
