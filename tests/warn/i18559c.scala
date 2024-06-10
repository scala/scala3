//> using options -Wall -Wunused:locals
// This test checks that -Wall leaves -Wunused:... untouched if it is already set
object FooImportUnused:
  import collection.mutable.Set // not warn

object FooUnusedLocal:
  def test(): Unit =
    val x = 1  // warn

object FooGivenUnused:
  import SomeGivenImports.given // not warn

object SomeGivenImports:
  given Int = 0
  given String = "foo"
