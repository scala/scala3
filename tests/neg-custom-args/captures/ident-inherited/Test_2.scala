package test

import caps.*

object Test extends A uses Consol:
  val a: () -> Unit = () => this.println()    // error
  val b: () -> Unit = () => this.print    // error
  val c: () -> Unit = () => println()    // error
  val d: () -> Unit = () => print    // error
