package tests

package classModifiers

sealed abstract class B

abstract case class C(s: String)

sealed case class D(c: String)

final case class E(c: String)

open class F

implicit class Foo(i: Int)
