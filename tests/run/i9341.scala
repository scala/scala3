class T { def f: Int = 10 }
class B extends T {
  class C { B.super[T].f }
  class D { B.super.f }
  new C
  new D
}

@main
def Test = new B
