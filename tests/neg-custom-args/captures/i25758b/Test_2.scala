import caps.*
import IO.*
def test1(): Unit =
  assertPure(() => IO.io.println("hello"))  // error, as expected
def test2(): Unit =
  def test(): Unit = IO.io.println("hello")
  assertPure: () =>  // error
    test()
