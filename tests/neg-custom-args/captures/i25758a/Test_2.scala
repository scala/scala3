package test
import caps.*
import IO.*
object Test uses IO:
  def test1(): Unit =
    assertPure(() => IO.io.println("hello"))  // error, as expected
  def test2(): Unit =
    def test(): Unit = IO.io.println("hello")
    assertPure: () =>  // error
      test()
