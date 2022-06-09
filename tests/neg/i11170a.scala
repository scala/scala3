package apackage {

  abstract class A {
    protected val x: Int
  }
  abstract class A2 {
    val x: Int
  }
}

package bpackage {
  import apackage._

  trait B extends A {
    println(x)
  }
  trait B2 extends A2 {
    println(x)
  }
}

package cpackage {
  import apackage._
  import bpackage._

  case class C(override protected val x: Int) extends A with B  // error
  case class C2(override val x: Int) extends A2 with B2
}