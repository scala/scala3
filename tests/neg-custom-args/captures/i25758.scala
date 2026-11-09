package test
import caps.*
object o extends SharedCapability:
  class IO extends SharedCapability:
    def println(msg: String): Unit = ()
  val io: IO^ = new IO
  def assertPure(op: () -> Unit): Unit = ()

import o.*
object Test uses test.o:
  def test1(): Unit =
    assertPure(() => io.println("hello"))  // error, as expected
  def test(): Unit = io.println("hello")
  def test2(): Unit =
    val f = () => this.test()
    assertPure(f) // error, as expected
    assertPure: () =>  // error
      this.test()
    assertPure: () =>  // error
      test()
  def test3(): Unit =
    def test(): Unit = io.println("hello")
    val f = () => this.test()
    assertPure(f) // error, as expected
    assertPure: () =>  // error
      this.test()
    assertPure: () =>  // error
      test()
