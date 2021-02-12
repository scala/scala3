package tests
package sealedClasses

sealed trait A

sealed trait B1 extends A
case class B2(a: Int) extends A
case object B3 extends A
class B4 extends A
object B4 extends A
object B5 extends A
case class B6(a: Int) extends A
case object B6 extends A

sealed trait C1 extends B1
case class C2(a: Int) extends B1
case object C3 extends B1
class C4 extends B1
object C4 extends B1
object C5 extends B1
case class C6(a: Int) extends B1
case object C6 extends B1
sealed trait C7 extends B1

