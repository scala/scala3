import language.experimental.erasedTerms
erased class A
erased class B extends A // ok

class C extends A  // error

erased trait D

val x = new A{} // ok, x is erased
val y = new C with D{} // error


