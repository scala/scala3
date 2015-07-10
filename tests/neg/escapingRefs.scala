object O {
  class A
  class B
  def f[T](x: T, y: T): T = y

  val x: A = f(new A { }, new B { })                  // error

  val y = f({ class C { def member: Int = 1 }; new C }, { class C { def member: Int = 1 }; new C })
  val z = y.member                                    // error
}
