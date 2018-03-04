class A(sealed val a: Int) // error
class B(lazy val a: Int) // error
class C(abstract val a: Int) // error
