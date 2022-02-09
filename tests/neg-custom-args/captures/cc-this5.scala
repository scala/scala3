class C:
  val x: C = this

@annotation.capability class Cap

def foo(c: Cap) =
  object D extends C:   // error
    def bar: Unit = println(c)
  object E:
    def bar: Unit = println(c)
  D.bar

def test(c: Cap) =
  class A:
    val x: A = this
    def f = println(c)  // error

def test2(c: Cap) =
  class A:
    def f = println(c) 
    val x: A = this // error
